
thumb.options <- c('_', 'p')
finger.options <- c('_', 'p')

random.thumb.choice <- function() {
    thumb.options[sample(1:length(thumb.options),1)]
}

random.finger.choice <- function() {
    finger.options[sample(1:length(finger.options),1)]
}

random.chord <- function() {
    paste(
        random.thumb.choice(),
        ",",
	random.finger.choice(),
	random.finger.choice(),
	random.finger.choice(),
	random.finger.choice(),
	sep = "")
}

random.chord()

