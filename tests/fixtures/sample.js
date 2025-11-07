// Sample JavaScript file for integration testing
function greet(name) {
    console.log("Hello, " + name);
    return "Hello, " + name;
}

function farewell(name) {
    console.log("Goodbye, " + name);
}

class Person {
    constructor(name) {
        this.name = name;
    }
    
    greet() {
        console.log("Hi, I'm " + this.name);
    }
}

const user = new Person("Alice");
user.greet();
greet("Bob");
farewell("Charlie");
