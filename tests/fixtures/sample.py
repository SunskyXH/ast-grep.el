# Sample Python file for integration testing
def greet(name):
    print(f"Hello, {name}")
    return f"Hello, {name}"

def farewell(name):
    print(f"Goodbye, {name}")

class Person:
    def __init__(self, name):
        self.name = name
    
    def greet(self):
        print(f"Hi, I'm {self.name}")

if __name__ == "__main__":
    user = Person("Alice")
    user.greet()
    greet("Bob")
    farewell("Charlie")
