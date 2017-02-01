Here is some header text.
We can have as many lines as we like, and these lines may include [local links](A0tr6X3) or [remote links](http://example.org).

# This is the first header

Header content within the first section.

The following is a list:

* one
* two
* buckle my shoe

# Links

## Embedded links

Here is an [embedded link](http://example.org) with a URL as target.

And here is an [embedded link](A0tr6X3) with an atom id as target.

Embedded links with an empty target, like [this one](), are tolerated.

## Line links

Each line may end with an address, using invisible text, like this one. [](A0tr6X3)

Here is a list of atoms with (hidden) references:

* Africa [](Hqoj7-0)
* Antarctica [](55OOdAl) 
* Asia [](rgXsJiG)
* Australia (continent) [](WCgQ9rU)
* Europe [](3iPI8mS) 
* North America [](f0u7rFD) 
* South America [](-qYu4zK)

Here is the same list using embedded links:

* [Africa](Hqoj7-0)
* [Antarctica](55OOdAl) 
* [Asia](rgXsJiG)
* [Australia (continent)](WCgQ9rU)
* [Europe](3iPI8mS) 
* [North America](f0u7rFD) 
* [South America](-qYu4zK)

## Header links

### Header with end-of-line link [](0-6kATH)

The above header has an invisible link at the end of the line.

### [Header with embedded link](0-6kATH) and suffix

The above header is itself a link, and should be clickable.

# Examples

This is a statement.  I should be able to make statements about this statement.

# Usage

Convert this page to HTML with
```bash
pandoc -f markdown brain/src/test/resources/net/fortytwo/smsn/brain/io/markdown/markdown-example-1.md -o markdown-example-1.html
```

# Miscellaneous

Here is some oddball syntax for testing.

##

The above should not be interpreted as a heading.

## Heading with no content