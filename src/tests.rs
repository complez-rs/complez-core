use crate::language::parse::AklParser;

#[test]
fn parse_numeric_with_boolean() {
    let mut ast: AklParser = AklParser::new();
    ast.run(
        "
        func test() {
            if ((v1 + 1) == ((v2*2)+1)) {
                call leave ()
                return
            }
        }
    ",
    );
}



#[test]
fn z3_test() {}
