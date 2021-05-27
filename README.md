# ParserGenerator
## Todo
### New Types and Restructuring.
 - Abstract 'zero_and_more 'one_or_more into a struct check into RangeBound and implement for that
 - Dual of Pair type (struct [parser1 and parser2]) aka Sum type (enum [parser1 or parser2]) also maybe something like implies type and not type to be fancy
 - Triple type (struct of three parsers)
 - make Input to the parser to have Iter trait
 - Check the lifetimes. it seems to work but not hundred percent sure if it actually does what i want it to do
 - Make a separate directory for string parsers.
 - Check if you can implement generic match literal and match any
 - get rid of the standalone parse trait implementations and put them into Parser structure or create a structure accordingly

### New Combinators


