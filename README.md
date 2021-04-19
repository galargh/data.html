# data.html [![Clojars Project](https://img.shields.io/clojars/v/org.clojars.gfjalar/data.html.svg)](https://clojars.org/org.clojars.gfjalar/data.html)

A Clojure library designed to tokenize HTML data.

The tokenization algorithm implemented by this library closely follows the specification given [here](https://html.spec.whatwg.org/multipage/parsing.html#tokenization).

The implementation of `tokenize` in this library returns a lazy sequence of tokens.

## Usage

```clojure
data.html=> (require 'org.clojars.gfjalar/data.html)
nil
data.html=> (data.html/tokenize "<html>")
({:type :start-tag, :data "html"} {:type :EOF})
data.html=> (data.html/tokenize "<html></html>")
({:type :start-tag, :data "html"} {:type :end-tag, :data "html"} {:type :EOF})
data.html=> (data.html/tokenize "<html><body><h1>data.html</h1></body></html>")
({:type :start-tag, :data "html"} {:type :start-tag, :data "body"} {:type :start-tag, :data "h1"} {:type :character, :data \d} {:type :character, :data \a} {:type :character, :data \t} {:type :character, :data \a} {:type :character, :data \.} {:type :character, :data \h} {:type :character, :data \t} {:type :character, :data \m} {:type :character, :data \l} {:type :end-tag, :data "h1"} {:type :end-tag, :data "body"} {:type :end-tag, :data "html"} {:type :EOF})
```

## [License](LICENSE)
