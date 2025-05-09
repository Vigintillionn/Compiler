use std::collections::{HashMap, HashSet};

use super::tac::{UsesDefs, TAC};

const WORD_SIZE: usize = 4; // Assuming 4 bytes for a word
const STACK_SIZE: usize = 1024;
// We limit ourselves to 7 registers for simplicity
// In a real scenario, this would be the number of registers available in the architecture
// The registers we use here are x5-x7 and x28-x31, which are temporary registers in RISC-V
// This is a simplification for the sake of this example, as we can ignore some calling conventions
const ALLOWED_REGISTERS: [usize; 7] = [5, 6, 7, 28, 29, 30, 31]; // x5-x7, x28-x31 which is t0-t2, t3-t6

pub type Allocation = HashMap<String, Location>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Location {
    Reg(usize),   // Physical register x#
    Spill(usize), // Stack slot index
}

pub trait RegisterAllocator {
    fn allocate_registers(&mut self) -> Allocation;
    fn compute_live_ranges(&self) -> HashMap<String, LiveRange>;
}

impl RegisterAllocator for TAC {
    fn allocate_registers(&mut self) -> Allocation {
        let live_ranges = self.compute_live_ranges();

        let mut ig = InterferenceGraph::new();
        for (temp, _) in &live_ranges {
            ig.add_node(temp.clone());
        }

        for (t1, range1) in &live_ranges {
            for (t2, range2) in &live_ranges {
                if t1 != t2 && range1.overlaps(range2) {
                    ig.add_edge(t1.clone(), t2.clone());
                }
            }
        }

        let coloring = ig.color();
        coloring
    }

    fn compute_live_ranges(&self) -> HashMap<String, LiveRange> {
        let mut ranges = HashMap::new();

        for (idx, instr) in self.instrs.iter().enumerate() {
            let defs = instr.defs();
            let uses = instr.uses();

            // Defs: mark start if new, extend end
            for def in defs {
                let entry = ranges.entry(def.clone()).or_insert(LiveRange(idx, idx));
                entry.1 = idx;
            }

            // Uses: extend end
            for u in uses {
                let entry = ranges.entry(u.clone()).or_insert(LiveRange(idx, idx));
                entry.1 = idx;
            }
        }

        ranges
    }
}

/// A simple representation of a live range for a variable.
#[derive(Debug, Clone)]
pub struct LiveRange(usize, usize);

impl LiveRange {
    fn overlaps(&self, other: &Self) -> bool {
        !(self.1 < other.0 || self.0 > other.1)
    }
}

#[derive(Default)]
struct InterferenceGraph {
    nodes: HashSet<String>,
    edges: HashMap<String, HashSet<String>>,
}

impl InterferenceGraph {
    fn new() -> Self {
        Self::default()
    }

    fn add_node(&mut self, v: String) {
        self.nodes.insert(v.clone());
        self.edges.entry(v).or_default();
    }

    fn add_edge(&mut self, u: String, v: String) {
        self.edges.get_mut(&u).unwrap().insert(v.clone());
        self.edges.get_mut(&v).unwrap().insert(u);
    }

    fn color(&self) -> Allocation {
        let mut result = HashMap::new();
        let mut ordered: Vec<_> = self.nodes.iter().cloned().collect();

        ordered.sort_by_key(|n| usize::max(self.edges.get(n).unwrap().len(), 0));

        for node in ordered {
            let mut forbidden = HashSet::new(); // x0 is reserved for zero, we'll use x5 and x6 for spill slots
            for neighbor in self.edges.get(&node).unwrap() {
                if let Some(color) = result.get(neighbor) {
                    match color {
                        Location::Reg(c) => {
                            forbidden.insert(*c);
                        }
                        Location::Spill(_) => {}
                    }
                }
            }

            let mut stack_idx = 0;
            for reg in ALLOWED_REGISTERS.iter() {
                if !forbidden.contains(reg) {
                    result.insert(node.clone(), Location::Reg(*reg));
                    break;
                }
            }
            // If no register is available, we need to spill
            if !result.contains_key(&node) {
                result.insert(node, Location::Spill(stack_idx));
                stack_idx += 1;
                // TODO: stack size should be handled by function itself and should result in an error instead of panic
                if stack_idx * WORD_SIZE >= STACK_SIZE {
                    panic!("Stack overflow: too many variables to spill");
                }
            }
        }
        result
    }
}
