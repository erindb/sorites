# cocomd

You can make basic HTML with slides and a couple kinds of inputs within a markdown-like syntax.

At the moment, the input files are hard-coded and lots of things are buggy and incomplete. If I end up using this in the future, it will hopefully get prettier...

## only a few syntax elements are implemented

* slides are delimited by `---` and have ids at the beginning, e.g. `--- intro`.
* `//` compiles to `<br/>`
* `___` will compile to a textbox.
* ` o o o ` will compile to a set of radio buttons with the same name.
* `[textbox N x M]` will compile to an NxM textarea.
* `###` will compile to `<h3>`
* `![alt](src)` will compile to an image with alt-text `alt` and source file `src`.
* `[[Button]]` will compile to a button with `.onclick('state.next()')`.
* `{{x}}` will compile to a span with `id='x'` and content `x`.
* if a line ends with `#something`, I do my best to assign that id to something reasonable (i.e. the closest input, or the paragraph)
