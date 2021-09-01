use crate::language::parse::AklParser;

#[test]
fn parse_numeric_with_boolean() {
    let mut ast: AklParser = AklParser::new();
    ast.run(
        "
        func test(v1: number, v2: number) {
            if (((v1 + 1) == (v2*2)) && ((v1*3) == (v2*5 + 3))) {
                    call leave ()
                    return
            }
        }
    ",
    );
    //dbg!(ast.funcs.clone());
    panic!("tt");
}

#[test]
fn z3_test() {}
