# Adib Lang

### Example:

```js
// sum of two numbers
fn sum(a, b){
    return a + b
}

var a = 1
var b = 2

var c = sum(a,b)

var i = 0
fn loop(){
    if(i<10){
        print("i is " + i)
        i = i + 1
        loop()
    }
}

loop()
```

## Functions:

- `print` : prints the value to the console
- `sqrt` : returns the square root of the number

### TODOS:

- [ ] Better error handling
