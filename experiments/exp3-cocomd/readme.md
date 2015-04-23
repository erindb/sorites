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

## example

~~~
--- questionnaire

	Answering these questions is optional, but will help us understand your answers.

	Did you read the instructions and do you think you did the HIT correctly? o No o Yes o I was confused #assess

	Gender: o Female o Male o Other #gender

	Age: ____ #age

	Level of Education: [ 0. Some High School
	                      1. Graduated High School
	                      2. Some College
	                      3. Graduated College
	                      4. Hold a higher degree ] #education

	Native Language: ____ #language

	Did you enjoy the hit? [ 0. Worse than the Average HIT
	                         1. An Average HIT
	                         2. Better than average HIT ] #enjoy

	We would be interested in any comments you have about this experiment. Please type them here:
	[textbox 3 x 50 ] #comments

	[[Submit]]

--- thanks

	Submitting to mTurk...	#submitting
~~~

will compile to:

<div class='slide' id='questionnaire'>

<p>Answering these questions is optional, but will help us understand your answers.</p>

<p>Did you read the instructions and do you think you did the HIT correctly?<input name='assess' type='radio' value='0'>No<input name='radio0' type='radio' value='0'>Yes<input name='radio1' type='radio' value='0'>I was confused </p>

<p>Gender:<input name='gender' type='radio' value='0'>Female<input name='radio2' type='radio' value='0'>Male<input name='radio3' type='radio' value='0'>Other </p>

<p>Age: <input type='text' id='age'></input> </p>

<p>Level of Education: <select id='education'><label><option value='-1'/></label><label><option value='0'/> Some High School </label><label><option value='1'/> Graduated High School </label><label><option value='2'/> Some College </label><label><option value='3'/> Graduated College </label><label><option value='4'/> Hold a higher degree  </label></select> </p>

<p>Native Language: <input type='text' id='language'></input> </p>

<p>Did you enjoy the hit? <select id='enjoy'><label><option value='-1'/></label><label><option value='0'/> Worse than the Average HIT </label><label><option value='1'/> An Average HIT </label><label><option value='2'/> Better than average HIT  </label></select> </p>

<p>We would be interested in any comments you have about this experiment. Please type them here:</p>
	<textarea id='comments' cols='50' rows='3'></textarea> 

<p><button type='button' onclick='state.next()'>Submit</button></p>

</div>

<div class='slide' id='thanks'>

<p id='submitting'>Submitting to mTurk...	</p>
