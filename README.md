# Adib Lang

### Examples:

```js
// Print "Hello, world!"
print("Hello, world!")

// Conditional statements
var age = 18
if (age >= 18) {
    print("You are eligible to vote.")
} else {
    print("You are not eligible to vote.")
}

// For loop
for (let i = 0; i  < 10; i++) {
  print(i)
}

// While loop
let i = 0
while (i < 10) {
  print(i)  
  i++
}

// Function definition
function add(a, b) {
  return a + b;
}

print(add(1, 2))  // Function call

// Arrays
var arr = [1, 2, 3, 4, 5]
print(arr[0])

// Objects
var obj = {
  name: "John",
  age: 18
}

print(obj.name)
```

## Standard Functions:

- `print`: Outputs the value to the console.
- `sqrt`: Returns the square root of the number.
- `free`: Deallocates the memory assigned to the variable.

## Editor

The Adib language is similar to JavaScript, so you can use any text editor to write Adib code.

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