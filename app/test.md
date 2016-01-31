# Thematic breaks

---
===
_  __ _ _ _
** ** **

Headers
====
----

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
----

    // Block 1
    var i = 0;
    console.log(i);

    // Block 2
    var i = 0;
      console.log(i);

      // Block 3
      var i = 0; console.log(i);

# Fenced code blocks
----

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

# Blockquotes
----

> Wow!
A paragraph inside a blockquote

# Lists
----

### Unordered

* One
* Two
* Three
* Four


* Uno

* Dos

* Tres

* Cuatro


### Ordered

1. One
2. Two
3. Three
4. Four


1) Uno

2) Dos

3) Tres

4) Cuatro

# Codespan
----

This is some `inline code`. Just like `this one` and ``th1s 0n3``

# Emphasis
----

**bold**

*italic*

**bold *italic***

*italic **bold***

**bold after **bold**

*italic after *italic*

***italic* bold**

***bold** italic*

*foo **bar *baz* bim** bop*

# Links
----

Follow the next [inline link](#next-section) to a same-page id.\
The following link contains [some **awesome** *styles*](http://www.google.com "Google").\
Link to [google][google] and [stack overflow][stacko] are referenced,\
just like the links to [**slack**][slack] and [**facebook**][facebook].

# Images
----

Here's the Haskell logo [![this is the alt text with some **style** and [links][google]][HASKELL logo]][haskell] and GitHub's Octocat [![octocat][octocat]][github]

# Line breaks
----

This paragraph contains a
soft break.

This one contains a\
hard break.

This one contains both
soft breaks and\
hard breaks.

# Link references
----

**Note**: They are not shown in HTML

[google]: http://www.google.com Google
[stacko]: http://www.stackoverflow.com
[slack]: http://www.slack.com Slack
[facebook]: http://www.facebook.com

[haskell]: https://www.haskell.org/ Haskell
[github]: https://github.com/ Github

[haskell logo]: https://www.haskell.org/static/img/logo.png?etag=rJR84DMh Haskell
[octocat]: https://camo.githubusercontent.com/fb782da4019ab66eeea35cc9b9ce73b2438b1688/687474703a2f2f646f632e72756c746f722e636f6d2f696d616765732f6769746875622d6c6f676f2e706e67
