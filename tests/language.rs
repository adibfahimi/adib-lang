use adib_lang::run_source;

fn run(source: &str) -> String {
    run_source(source).unwrap_or_else(|error| panic!("program failed: {}", error))
}

fn fails_with(source: &str, expected: &str) {
    let error = run_source(source).expect_err("program should have failed");
    assert!(
        error.contains(expected),
        "expected error containing `{}`, got `{}`",
        expected,
        error
    );
}

#[test]
fn prints_values() {
    assert_eq!(run("print(\"hello, world!\")"), "hello, world!\n");
    assert_eq!(run("print(1, true, null)"), "1 true null\n");
}

#[test]
fn respects_operator_precedence() {
    assert_eq!(
        run("
            print(1 + 2 * 3)
            print((1 + 2) * 3)
            print(10 - 4 - 3)
            print(8 / 2 / 2)
            print(7 % 4)
        "),
        "7\n9\n3\n2\n3\n"
    );
}

#[test]
fn supports_unary_operators() {
    assert_eq!(run("print(-(2 + 3))\nprint(!false)"), "-5\ntrue\n");
}

#[test]
fn supports_strings_and_escapes() {
    assert_eq!(
        run(r#"print("line\nbreak")
print("tab\there")
print("say \"hi\"")"#),
        "line\nbreak\ntab\there\nsay \"hi\"\n"
    );
}

#[test]
fn supports_comments() {
    assert_eq!(
        run("
            // line comment
            print(8 / 2) /* block comment */
            /* multi
               line */
            print(3)
        "),
        "4\n3\n"
    );
}

#[test]
fn branches_and_loops() {
    let source = "
        var total = 0
        for (var i = 1; i <= 5; i++) {
            if (i % 2 == 0) {
                total = total + i
            }
        }

        var countdown = 3
        while (countdown > 0) {
            total = total + countdown
            countdown = countdown - 1
        }

        if (total == 12) {
            print(\"ok\")
        } else {
            print(\"unexpected\")
        }
    ";
    assert_eq!(run(source), "ok\n");
}

#[test]
fn supports_arrays_and_string_indexing() {
    let source = "
        const numbers = [1, 2, 3, 4]
        print(numbers.len)
        print(numbers[2])
        print(numbers[0] + numbers[3])
        print(\"adib\"[1])
        print([1, 2] + [3])
    ";
    assert_eq!(run(source), "4\n3\n5\nd\n[1, 2, 3]\n");
}

#[test]
fn functions_can_recurse_and_return_arrays() {
    let source = "
        function two_sum(nums, target) {
            for (var i = 0; i < nums.len; i++) {
                for (var j = i + 1; j < nums.len; j++) {
                    if (nums[i] + nums[j] == target) {
                        return [i, j]
                    }
                }
            }
            return null
        }
        print(two_sum([2, 7, 11, 15], 9))
        print(two_sum([1, 2], 99))
    ";
    assert_eq!(run(source), "[0, 1]\nnull\n");
}

#[test]
fn functions_are_closures() {
    let source = "
        function make_counter() {
            var count = 0
            function increment() {
                count = count + 1
                return count
            }
            return increment
        }

        const counter = make_counter()
        print(counter())
        print(counter())
        print(counter())
    ";
    assert_eq!(run(source), "1\n2\n3\n");
}

#[test]
fn blocks_have_their_own_scope() {
    let source = "
        var value = 1
        if (true) {
            var value = 2
            print(value)
        }
        print(value)
    ";
    assert_eq!(run(source), "2\n1\n");
}

#[test]
fn variables_default_to_null() {
    assert_eq!(run("var x\nprint(x)\nprint(x == null)"), "null\ntrue\n");
}

#[test]
fn bare_return_returns_null() {
    assert_eq!(
        run("function nothing() { return }\nprint(nothing())"),
        "null\n"
    );
}

#[test]
fn runtime_errors_are_reported() {
    fails_with("print(missing)", "undefined variable");
    fails_with("missing()", "undefined function");
    fails_with("print(1 / 0)", "division by zero");
    fails_with("print(1 % 0)", "division by zero");
    fails_with("const x = 1\nx = 2", "constant");
    fails_with("if (1) { print(\"no\") }", "expects a boolean");
    fails_with("print(1 + \"a\")", "invalid binary operation");
    fails_with("print([1][2])", "index out of bounds");
    fails_with("print(-1[0])", "index");
    fails_with("function add(a) { return a }\nadd()", "expects 1 argument");
    fails_with("return 1", "outside of a function");
    fails_with("print(1.length)", "use `.len`");
}

#[test]
fn syntax_errors_include_positions() {
    let error = run_source("if (true { print(1) }").expect_err("program should fail");
    assert!(error.contains("1:10"), "unexpected error: {}", error);
}

#[test]
fn showcase_program_matches_golden_output() {
    assert_eq!(
        run(include_str!("../examples/showcase.adib")),
        include_str!("fixtures/showcase.out")
    );
}

#[test]
fn examples_run() {
    assert_eq!(
        run(include_str!("../examples/hello_world.adib")),
        "hello world!\n"
    );
    assert_eq!(run(include_str!("../examples/two_sum.adib")), "[0, 1]\n");
    assert_eq!(
        run(include_str!("../examples/fizzbuzz.adib")),
        "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n"
    );
    assert_eq!(run(include_str!("../examples/closures.adib")), "1\n2\n3\n");
    assert_eq!(
        run(include_str!("../examples/fibonacci.adib")),
        "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n"
    );
}
