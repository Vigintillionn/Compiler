use crate::{errors::ReportableError, sourcemap::SourceMap};

pub trait Pipeline<T> {
    fn with_ctx<O, E, F>(self, src: &SourceMap, f: F) -> Result<O, String>
    where
        O: std::fmt::Debug,
        E: ReportableError,
        F: Fn(T) -> Result<O, Vec<E>>;
}

impl<T> Pipeline<T> for T {
    fn with_ctx<O, E, F>(self, src: &SourceMap, f: F) -> Result<O, String>
    where
        O: std::fmt::Debug,
        E: ReportableError,
        F: Fn(T) -> Result<O, Vec<E>>,
    {
        handle_errors(self, src, f)
    }
}

impl<T> Pipeline<T> for Result<T, String> {
    fn with_ctx<O, E, F>(self, sm: &SourceMap, f: F) -> Result<O, String>
    where
        O: std::fmt::Debug,
        E: ReportableError,
        F: Fn(T) -> Result<O, Vec<E>>,
    {
        match self {
            Ok(val) => handle_errors(val, sm, f),
            Err(err) => Err(err),
        }
    }
}

pub fn handle_errors<F, E, I, O>(input: I, src: &SourceMap, f: F) -> Result<O, String>
where
    O: std::fmt::Debug,
    E: ReportableError,
    F: Fn(I) -> Result<O, Vec<E>>,
{
    let out = f(input);
    let Ok(out) = out else {
        let errors = out.unwrap_err();
        let len = errors.len();
        for e in errors {
            e.report(src);
        }
        eprintln!("Found {} errors", len);

        return Err("Failed to compile".to_string());
    };

    Ok(out)
}
