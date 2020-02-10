## Dijkstra notation

"Var" is a variable whose value is a Dijkstra array

### Accessors

var**.lob** {lower bound}  
var**.upb** {upper bound}  
var**.dom** {hib - lob}  
var(n) or var**.val**(n) {get nth element}  
var**.low** {var.val(var.lob)}  
var**.high** {var.val(var.hib)}  

## Mutators

var**:shift**(n) {translate indices up by n}  
var**:hiext**(x) {extend upper end with value x}  
var**:loext**(x) {extend lower end with value x}  
var**:hirem** {remove highest element and return it}  
var**:lorem** {remove lowest element and return it}  
var**:swap**(i, j) {swap ith and jth elements atomically}  
var**:alt**(i, x) {atomically replace ith element with x and return old ith element}  
var:(i) := x {replace nth element with x}  
var := (x1, x2, x3) {assign elements to var, lob becomes 0}

## Procedures

## Implementation

Use a circular buffer so that everything is O(1).
When doing **hiext** or **loext** and the buffer is full,
copy to a buffer twice as large so that the cost is amortized O(1).
