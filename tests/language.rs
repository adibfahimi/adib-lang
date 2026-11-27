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
fn supports_break_and_continue() {
    assert_eq!(
        run("
            var values = []
            for (var i = 0; i < 10; i++) {
                if (i == 2) { continue }
                if (i == 6) { break }
                values = push(values, i)
            }
            print(values)

            var n = 0
            var sum = 0
            while (n < 10) {
                n += 1
                if (n % 2 == 0) { continue }
                if (n > 7) { break }
                sum += n
            }
            print(sum)

            var pairs = []
            for (var row = 0; row < 3; row++) {
                for (var column = 0; column < 3; column++) {
                    if (column == 1) { continue }
                    if (column == 2) { break }
                    pairs = push(pairs, row * 10 + column)
                }
            }
            print(pairs)
        "),
        "[0, 1, 3, 4, 5]\n16\n[0, 10, 20]\n"
    );
}

#[test]
fn supports_compound_and_update_assignment() {
    assert_eq!(
        run("
            var x = 10
            x += 5
            x -= 3
            x *= 2
            x /= 4
            x %= 4
            x++
            x--
            print(x)

            var text = \"a\"
            text += \"b\"
            print(text)

            var values = [1]
            values += [2]
            print(values)

            for (var i = 0; i < 5; i += 2) {
                print(i)
            }
        "),
        "2\nab\n[1, 2]\n0\n2\n4\n"
    );
}

#[test]
fn supports_index_assignment() {
    assert_eq!(
        run("
            var values = [1, 2, 3]
            values[0] = 10
            values[1] += 5
            values[2]++
            print(values)

            const grid = [[1, 2], [3, 4]]
            grid[1][0] = 30
            grid[0][1] -= 1
            print(grid)

            const fixed = [1, 2]
            fixed[0] = 9
            print(fixed)

            var calls = 0
            function next_index() {
                calls += 1
                return 0
            }
            var single = [10]
            single[next_index()] += 5
            print(calls, single)
        "),
        "[10, 7, 4]\n[[1, 1], [30, 4]]\n[9, 2]\n1 [15]\n"
    );
}

#[test]
fn standard_library_works() {
    assert_eq!(
        run("
            const values = [1, 2, 3]
            print(push(values, 4))
            print(values)
            print(pop(values))
            print(last(values))
            print(slice(values, 1))
            print(slice(values, 0, 2))
            print(slice(\"adib\", 1, 3))
            print(join(values, \"-\"))
            print(split(\"a,b,c\", \",\"))
            print(split(\"abc\", \"\"))
            print(to_string([1, 2]))
            print(parse_number(\" 3.5 \"))
            print(parse_number(\"nope\"))
            print(abs(-7), floor(3.9), ceil(3.1), round(2.5), sqrt(81))
            print(min(3, 5), max(3, 5))
            print(len(\"héllo\"))
            print(type_of(values))
        "),
        "[1, 2, 3, 4]\n[1, 2, 3]\n[1, 2]\n3\n[2, 3]\n[1, 2]\ndi\n1-2-3\n[a, b, c]\n[a, b, c]\n[1, 2]\n3.5\nnull\n7 3 4 3 9\n3 5\n5\narray\n"
    );
}

#[test]
fn standard_library_reports_errors() {
    fails_with("push(1, 2)", "expects an array");
    fails_with("pop([])", "empty array");
    fails_with("last([])", "empty array");
    fails_with("slice(1, 0)", "array or string");
    fails_with("join([1], 2)", "expects a string");
    fails_with("parse_number(1)", "expects a string");
    fails_with("sqrt(-1)", "negative");
    fails_with("abs()", "expects 1 argument");
}

#[test]
fn break_and_continue_only_work_inside_loops() {
    fails_with("break", "`break` outside of a loop");
    fails_with("continue", "`continue` outside of a loop");
    fails_with(
        "function stop() { break }\nfor (var i = 0; i < 2; i++) { stop() }",
        "`break` outside of a loop",
    );
    fails_with(
        "function skip() { continue }\nfor (var i = 0; i < 2; i++) { skip() }",
        "`continue` outside of a loop",
    );
}

#[test]
fn assignment_errors_are_reported() {
    fails_with("var values = [1]\nvalues[2] = 3", "index out of bounds");
    fails_with(
        "var text = \"a\"\ntext[0] = \"b\"",
        "cannot index into string",
    );
    fails_with("var value = 1\nvalue.field = 2", "can only assign");
    fails_with("const answer = 42\nanswer += 1", "constant");
}

#[test]
fn diagnostics_include_source_context_and_stack_trace() {
    let error = run_source("function divide(a, b) {\n  return a / b\n}\nprint(divide(1, 0))")
        .expect_err("program should fail");

    assert!(error.contains("division by zero"), "{}", error);
    assert!(error.contains("return a / b"), "{}", error);
    assert!(error.contains("--> 2:12"), "{}", error);
    assert!(error.contains("^"), "{}", error);
    assert!(error.contains("stack trace:"), "{}", error);
    assert!(error.contains("at `divide`"), "{}", error);
}

#[test]
fn stack_traces_include_nested_calls_innermost_first() {
    let error =
        run_source("function outer() { middle() }\nfunction middle() { return 1 / 0 }\nouter()")
            .expect_err("program should fail");

    let middle = error.find("at `middle`").expect("middle frame");
    let outer = error.find("at `outer`").expect("outer frame");
    assert!(middle < outer, "{}", error);
}

#[test]
fn syntax_errors_name_the_expected_and_found_tokens() {
    let error = run_source("if (true { print(1) }").expect_err("program should fail");
    assert!(error.contains("expected `)`, found `{`"), "{}", error);
    assert!(error.contains("1 | if (true { print(1) }"), "{}", error);
    assert!(error.contains("^"), "{}", error);
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
