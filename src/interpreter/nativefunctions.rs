use super::Value;

pub fn print(args: Vec<Value>) -> Value {
    let mut output = String::new();
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            output.push_str(", ");
        }

        output.push_str(&format!("{}", arg));
    }
    println!("{}", output);
    Value::Void
}
