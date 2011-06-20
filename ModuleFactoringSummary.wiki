
Here is a short summary of Module factoring proposals :

||'''Feature'''        ||   '''R5RS'''  || '''ModuleFactoringCowan'''                ||   '''ModuleFactoringShinn'''                   ||     '''ModuleFactoringGleckler'''               ||       '''ModuleFactoringMedernach''' ||
||(scheme io)      || base      || base                   || (scheme io base)            || (scheme io)                 || (scheme ports) ||
||(scheme repl)    || base      || yes                    || no                          || yes                         || yes ||
||case-lambda      || SRFI 16   || base                   || (scheme case-lambda)        || (scheme case-lambda)        || (scheme arguments case-lambda) ||
||multiple values  || base      || base                   || (scheme multiple-values)    || base                        || base ||
||normalisation    || none      || (scheme char normalization) || (scheme char normalization) || (scheme char normalization) || (scheme char normalization) ||
||package all io   || base      || no                     || (scheme io)                 || no                          || no ||
||parameters       || SRFI 39   || base                   || base                        || base                        || (scheme parameters) ||
||records          || none      || base                   || base                        || base                        || (scheme records srfi-9) ||
||unicode          || base      || (scheme char)          || (scheme char)               || (scheme char)               || (scheme char) ||
