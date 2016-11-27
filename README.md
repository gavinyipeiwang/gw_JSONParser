# gw_JSONParser
Implement a JSON parser.

The main purpose of this exercise is to get myself better understand parsing techniques. It's not meant to be a serious implementation as there are a lot of good ones already. The reason I choose JSON rather than a toy language is because of the simplicity of its grammar. Besides its grammar is context-free, which means I could write a recursive descent parser. For the details of JSON grammar and its format, please refer to http://www.json.org/. 

Unlike a common parser whose parsing process is broken down into two steps: tokenizing and parsing, a JSON parser only needs one step to parse an input. It's because each nonterminal symbol in ita grammar is a JSON value, and it does not have any leaves, therefore it can be returned directly to form the result JSON object. I found it's not difficult to write a JSON parser once you understand general parsing techniques. The challenge comes to error handling and performance optimization.



