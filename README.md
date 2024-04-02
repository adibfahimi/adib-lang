# Adib Lang

### Example:

```js
const myObject = {
  name: "Adib Fahimi",
  age: 19,
};

print("Hey, I'm ", myObject.name, " and I'm ", myObject.age, " years old!");

const myArray = [1, 2, 61213, 444, 5];

// Calculate the average of the array
let sum = 0;
for (let i = 0; i < myArray.length; i = i + 1) {
  sum = sum + myArray[i];
}

print("The average of the array is: ", sum / myArray.length);

const a = 9;

if (a > 10) {
  print("a is greater than 10");
} else {
  print("a is less than 10");
}
```

## Functions:

- `print`: prints the value to the console
- `sqrt`: returns the square root of the number
- `free`: frees the memory allocated to the variable

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