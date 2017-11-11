## Factoring Module Proposals

This is an attempt to factor R6RS, [ModulesShinn](ModulesShinn.md), the `library` and `module` parts of [ModulesAndPackagesArcfide](ModulesAndPackagesArcfide.md), and [ModulesGanz](ModulesGanz.md) into features that they have or don't have, to make decisions easier.  I have neglected purely syntactic features like the names and ordering constraints of forms and keywords.  I've added columns for a few existing Schemes.

|**Feature**|**r6rs**|**shinn**|**hsu-library**|**hsu-module**|**ganz**|**chicken**|**chez**|**PLT**|
|**Module type**|Static|Static|Syntactic|Syntactic|Syntactic|Syntactic|Syntactic|Static|
|**Top-level only**|Yes|Yes|Yes|No|No|Yes|No|Yes|
|**Name**|List|List|List|Identifier|Identifier|Identifier|Identifier||
|**Anonymous self-importing modules**|No|No|No|Yes|Yes|No|Yes||
|**File inclusion within module**|In most cases|Yes|Yes|Yes|No|Yes|Yes|Yes|
|**Body**|Implicit|Explicit|Implicit|Implicit|Implicit|Implicit|Implicit||
|**Exports syntax forms?**|Yes|Yes|Yes|Yes|Yes|Yes|Yes|Yes|
|**Import qualifiers: `only` `except` `rename` `prefix`**|Yes|Yes|Yes|Yes|Yes|Yes|Yes (also strip prefixes)||
|**Rename on export**|Yes|Yes|Yes|Yes|No|No|No||
|**Macros can generate modules**|No|No|Unclear|Yes|Unspecified restrictions|Yes|Yes|No|
|**Support for `co-export`**|No|No|Yes|Yes|No|No|No||
|**Implicit exports**|Automatic|Must be explicit|Automatic/Overridable|Automatic/Overridable|Automatic/Overridable|Must be explicit|???||
|**Import into REPL**|No|Yes|Yes|Yes|Yes|Yes|Yes|Yes|
|**Phasing**|Yes|No|No|No|No|Partial|Unclear||
|**Versioning**|Yes|No|No|No|No|No|No||
|**Strict subset of R6RS**|Obviously|Yes|Yes|No|No|No|No|No|
|**Supports parameterization of modules by imports**|No|No|No|No|Yes|No|No|Yes|

The difference between R6RS and R6RS-- is that R6RS-- does not have phasing or versioning.
