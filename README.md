# Adib Lang

### Example:

```js
// sum of two numbers
function sum(a, b) {
  return a + b;
}

var a = 1;
var b = 2;

var c = sum(a, b);

for (var i = 0; i < 10; i = i + 1) {
  print(i);
}

while (a < 10) {
  print(a);
  a = a + 1;
}

if (a > 10) {
  print("a is greater than 10");
} else {
  print("a is less than 10");
}
```

## Functions:

- `print`: prints the value to the console
- `sqrt`: returns the square root of the number

## Editor

Since this is the Adib language and it is similar to JavaScript, you can use any text editor to write Adib code.

- ### Neovim

```lua
vim.cmd([[
au BufNewFile,BufRead *.adib set filetype=javascript
]])

```

- ### VSCode `settings.json`

```json
"files.associations": {
    "*.adib": "javascript"
}
```

### TODOS:

- [ ] Better error handling
