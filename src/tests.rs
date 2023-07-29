use crate::assembler::*;
use crate::*;
use rand::rngs::StdRng;
use rand::{random, Rng, SeedableRng};
use std::collections::{BTreeSet, HashSet};
use std::mem::{size_of, size_of_val};

#[test]
fn test_compile() {
    let text = r#"
        nop ; must be at start cause core starts execution from word 1 (address 2)
        jmp @Start
        Func:
            mov r1,#-1
            jmp r14

        Start:
        call r14,@Func
        mov r2,#-2



        "#;

    let ops = compile_assembly(text).unwrap_or_else(|err| {
        println!("{}", err);
        Vec::new()
    });
    //println!("{:?}",ops);

    let mut vm = VirtualMachine::new(1024 * 4);
    vm.load_start(ops);
    vm.tick_times(10, false, true);
    println!("{:#?}", vm.cpu());
    println!("{}", vm.ram()[532]);
}

#[test]
fn test_compare() {
    fn print_truth_table() {
        fn v(b: bool) -> &'static str {
            if b {
                "1"
            } else {
                "0"
            }
        }
        println!("***Truth table:");
        println!("{:>3} {:>3} {:>3} {:>3}", "res", "av", "bv", "cv");
        for av in [false, true].iter().copied() {
            for bv in [false, true].iter().copied() {
                for cv in [false, true].iter().copied() {
                    let res = ((av && bv && !cv) || (!av && !bv && cv)) != cv; //expression
                    println!("{:>3} {:>3} {:>3} {:>3}", v(res), v(av), v(bv), v(cv));
                }
            }
        }
    }
    print_truth_table();
    type Data = u8;

    for a in Data::MIN..=Data::MAX {
        for b in Data::MIN..=Data::MAX {
            let (diff, carry) = a.overflowing_sub(b);
            let s = {
                let m = 1 << (size_of_val(&diff) * 8 - 1);
                let av = a & m != 0;
                let bv = (!b) & m != 0;
                let cv = diff & m != 0;

                // overflow flat xor sign
                ((av && bv && !cv) || (!av && !bv && cv)) != cv
            };
            let zero = diff == 0;

            let eq = zero;
            let ne = !zero;
            let lt = carry;
            let ge = !carry;
            let lts = s;
            let ges = !s;

            assert_eq!(a < b, lt, "Failed a < b {} {}", a, b);
            assert_eq!(a >= b, ge, "Failed a >= b {} {}", a, b);
            assert_eq!((a as i8) < (b as i8), lts, "Failed signed a < b {} {}", a as i8, b as i8);
            assert_eq!((a as i8) >= (b as i8), ges, "Failed signed a >= b {} {}", a as i8, b as i8);
            assert_eq!(a == b, eq, "Failed a == b {} {}", a, b);
            assert_eq!(a != b, ne, "Failed a != b {} {}", a, b);
        }
    }
    assert!(false);
}

#[test]
fn random_test_assert() {
    let mut rng = StdRng::seed_from_u64(4201);

    let mut vm = VirtualMachine::new(1024 * 32);
    rng.fill(vm.ram_mut());
    vm.tick_times(10000, false, true);
    println!("{:#?}", vm.cpu());
}
