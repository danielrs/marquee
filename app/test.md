# Thematic breaks

---
===
_  __ _ _ _
** ** **

Headers
====

# One
## Two
### Three
#### Four
##### Five
###### Six

One
====
Two
----

# Indented code blocks

    // Block 1
    var i = 0;
    console.log(i);

    // Block 2
    var i = 0;
      console.log(i);

      // Block 3
      var i = 0; console.log(i);

# Fenced code blocks

```javascript
var i = 0;
console.log(i);
console.log(++i);
```

   ```haskell
   factorial :: Int -> Int
   factorial 0 = 1
   factorial n = n * factorial (n - 1)
   ```

# Link reference
The following blocks are removed from the AST; so they are not shown.

[link1](www.google.com)
[link2](www.google.com 'Google')

Follow the next [link](#next-section)

The following link contains [some **awesome** *styles* __like _woaw___](www.stylish.com "Stylish")

# Emphasis tests

**bold**

*italic*

**bold *italic***

*italic **bold***

**bold after **bold**

*italic after *italic*

***italic* bold**

***bold** italic*

*foo **bar *baz* bim** bop*
and another paragraph line here
Just because **why not**? this looks awesome

# Blockquotes

> Wow!
Awesome
Just another
Blockquote stuff

# Lists

* One
* Two
* Three
* Four


* Uno

* Dos

* Tres

* Cuatro


1. One
2. Two
3. Three
4. Four


1) Uno

2) Dos

3) Tres

4) Cuatro
