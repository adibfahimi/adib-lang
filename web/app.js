import init, { run } from "./pkg/adib_lang.js";

const DEFAULT_CODE = `// Adib Lang — edit anything and press Run.
function greet(name) {
  return "hello, " + name
}

function fibonacci(n) {
  var a = 0
  var b = 1
  for (var i = 0; i < n; i++) {
    const next = a + b
    a = b
    b = next
  }
  return a
}

const names = ["world", "GitHub", "recruiter"]
for (var i = 0; i < names.len; i++) {
  print(greet(names[i]))
}

print("fib(10) =", fibonacci(10))`;

const EXAMPLES = {
  hello: {
    label: "Hello, world",
    code: `print("Hello, world!")

const name = "Adib Lang"
print("Welcome to", name)`,
  },
  two_sum: {
    label: "Two Sum",
    code: `function two_sum(nums, target) {
  for (var i = 0; i < nums.len; i++) {
    for (var j = i + 1; j < nums.len; j++) {
      if (nums[i] + nums[j] == target) {
        return [i, j]
      }
    }
  }
  return null
}

const nums = [2, 7, 11, 15]
print("array =", nums)
print("indices =", two_sum(nums, 9))
print("missing =", two_sum(nums, 100))`,
  },
  fizzbuzz: {
    label: "FizzBuzz",
    code: `for (var i = 1; i <= 15; i++) {
  if (i % 15 == 0) {
    print("FizzBuzz")
  } else if (i % 3 == 0) {
    print("Fizz")
  } else if (i % 5 == 0) {
    print("Buzz")
  } else {
    print(i)
  }
}`,
  },
  closures: {
    label: "Closures",
    code: `function make_counter() {
  var count = 0

  function increment() {
    count = count + 1
    return count
  }

  return increment
}

const counter = make_counter()
print("first  =", counter())
print("second =", counter())
print("third  =", counter())`,
  },
  recursion: {
    label: "Recursive Fibonacci",
    code: `function fib(n) {
  if (n < 2) {
    return n
  }
  return fib(n - 1) + fib(n - 2)
}

for (var i = 0; i <= 10; i++) {
  print("fib of", i, "=", fib(i))
}`,
  },
  mutation: {
    label: "Mutation & control flow",
    code: `var odd_values = []
for (var i = 1; i <= 10; i++) {
  if (i % 2 == 0) { continue }
  if (i > 7) { break }
  odd_values = push(odd_values, i)
}
print("odd values =", odd_values)

var scores = [72, 90, 65]
scores[0] += 8
scores[2] -= 5
print("adjusted =", scores)`,
  },
  stdlib: {
    label: "Standard library",
    code: `const words = split("rust,wasm,interpreter", ",")
print("words =", words)
print("count =", len(words))
print("joined =", join(words, " + "))
print("first two =", slice(words, 0, 2))
print("last =", last(words))
print("pushed =", push(words, "playground"))
print("parsed =", parse_number("3.14"))
print("sqrt(81) =", sqrt(81))
print("type =", type_of(words))`,
  },
  matrix: {
    label: "Matrix multiplication",
    code: `function multiply(left, right) {
  var result = []
  for (var row = 0; row < left.len; row++) {
    var output_row = []
    for (var column = 0; column < right[0].len; column++) {
      var cell = 0
      for (var k = 0; k < right.len; k++) {
        cell = cell + left[row][k] * right[k][column]
      }
      output_row = output_row + [cell]
    }
    result = result + [output_row]
  }
  return result
}

const a = [[1, 2], [3, 4]]
const b = [[5, 6], [7, 8]]
print("A =", a)
print("B =", b)
print("A * B =", multiply(a, b))`,
  },
};

const STORAGE_KEY = "adib-lang:source";

const textarea = document.getElementById("editor");
const examplesSelect = document.getElementById("examples");
const runButton = document.getElementById("run");
const clearButton = document.getElementById("clear");
const statusElement = document.getElementById("status");
const statusText = document.getElementById("status-text");
const outputElement = document.getElementById("output");
const outputMeta = document.getElementById("output-meta");

let editor = null;
let execute = null;
let running = false;

function getCode() {
  return editor ? editor.getValue() : textarea.value;
}

function setCode(value) {
  if (editor) {
    editor.setValue(value);
    editor.clearHistory();
    editor.refresh();
  } else {
    textarea.value = value;
  }
}

function setStatus(state, message) {
  statusElement.dataset.state = state;
  statusText.textContent = message;
}

function setOutput(text, isError) {
  outputElement.textContent = text;
  outputElement.classList.toggle("is-error", isError);
}

function saveCode() {
  try {
    localStorage.setItem(STORAGE_KEY, getCode());
  } catch {
    // Storage can be unavailable in private browsing; the playground still works.
  }
}

function loadCode() {
  try {
    return localStorage.getItem(STORAGE_KEY);
  } catch {
    return null;
  }
}

function finishRun(result, startedAt) {
  const elapsed = Math.max(0.1, performance.now() - startedAt);
  const isError = result.startsWith("error: ");
  const body = isError ? result.slice("error: ".length).trim() : result;
  const lineCount = body.split("\n").filter((line) => line.length > 0).length;

  setOutput(body || (isError ? "Unknown error" : "(no output)"), isError);
  outputMeta.textContent = `${lineCount} line${lineCount === 1 ? "" : "s"} · ${elapsed.toFixed(1)} ms`;
  setStatus(isError ? "error" : "ready", isError ? "Error" : `Finished in ${elapsed.toFixed(1)} ms`);

  saveCode();
  running = false;
  runButton.disabled = execute === null;
}

function runCode() {
  if (!execute || running) {
    return;
  }

  running = true;
  runButton.disabled = true;
  setStatus("running", "Running…");

  const startedAt = performance.now();

  // Yield once so the status change paints before synchronous execution.
  requestAnimationFrame(() => {
    setTimeout(() => {
      try {
        finishRun(execute(getCode()), startedAt);
      } catch (error) {
        setOutput(String(error), true);
        outputMeta.textContent = "interpreter panic";
        setStatus("error", "Interpreter panic");
        running = false;
        runButton.disabled = execute === null;
      }
    }, 0);
  });
}

function clearOutput() {
  setOutput("", false);
  outputMeta.textContent = "cleared";
  setStatus(execute ? "ready" : "loading", execute ? "Ready" : "Loading interpreter…");
}

function initializeEditor() {
  if (!window.CodeMirror) {
    return;
  }

  editor = window.CodeMirror.fromTextArea(textarea, {
    mode: "javascript",
    theme: "material-darker",
    lineNumbers: true,
    lineWrapping: false,
    indentUnit: 2,
    tabSize: 2,
    viewportMargin: Infinity,
    autoCloseBrackets: true,
    matchBrackets: true,
    extraKeys: {
      "Cmd-Enter": runCode,
      "Ctrl-Enter": runCode,
      Tab: (instance) => instance.replaceSelection("  "),
    },
  });

  editor.setSize("100%", "100%");
}

function initializeExamples() {
  for (const [key, example] of Object.entries(EXAMPLES)) {
    const option = document.createElement("option");
    option.value = key;
    option.textContent = example.label;
    examplesSelect.append(option);
  }

  examplesSelect.addEventListener("change", () => {
    const example = EXAMPLES[examplesSelect.value];
    if (!example) {
      return;
    }
    setCode(example.code);
    examplesSelect.value = "";
    runCode();
  });
}

function initializeShortcuts() {
  document.addEventListener("keydown", (event) => {
    if ((event.metaKey || event.ctrlKey) && event.key === "Enter") {
      event.preventDefault();
      runCode();
    }
  });

  textarea.addEventListener("keydown", (event) => {
    if (editor || event.key !== "Tab") {
      return;
    }
    event.preventDefault();
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    textarea.value = `${textarea.value.slice(0, start)}  ${textarea.value.slice(end)}`;
    textarea.selectionStart = textarea.selectionEnd = start + 2;
  });
}

async function boot() {
  try {
    await init();
    execute = run;
    runButton.disabled = false;
    setStatus("ready", "Ready");
    runCode();
  } catch (error) {
    console.error(error);
    setStatus("error", "Interpreter failed to load");
    setOutput(
      `Could not load the WebAssembly module.\n\n${String(error)}\n\nServe this page over HTTP instead of opening the HTML file directly.`,
      true,
    );
    outputMeta.textContent = "wasm load error";
  }
}

initializeEditor();
initializeExamples();
initializeShortcuts();

const savedCode = loadCode();
if (savedCode) {
  setCode(savedCode);
} else {
  setCode(DEFAULT_CODE);
}

runButton.addEventListener("click", runCode);
clearButton.addEventListener("click", clearOutput);

// Small surface for debugging and browser-console experiments.
window.adibPlayground = {
  run: runCode,
  getCode,
  setCode,
  editor: () => editor,
};

boot();
