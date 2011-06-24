
################################################################################
# reusable code

h <- read.table(file("/Users/josh/projects/fortytwo/mob-data/flashcards/flashcards-history.txt"), sep="\t", comment.char="", quote="", col.names=c("deck", "card", "time", "result"))

#trials <- data.frame(deck=h$deck, card=h$card, time=h$time, result=h$result == "Correct")
trials <- data.frame(deck=h$deck, card=paste(h$deck, h$card), time=h$time, result=h$result == "Correct")

cards <- unique(trials$card)
decks <- unique(trials$deck)
decks.and.cards <- unique(data.frame(deck=trials$deck, card=trials$card))

# Note data frame is assume to be ordered by increasing time
x <- trials[with(trials, order(trials$card)),]
first.trials <- x[!duplicated(x$card),]

x <- trials[with(trials, order(-trials$time)),]
last.trials <- x[!duplicated(x$card),]


########################################

first.trial.time <- function(c) {
    min(subset(trials, trials$card == c)$time)
}

deck.proportion.correct.on.first.try <- function(d) {
    nrow(subset(first.trials, deck == d & result)) / nrow(subset(decks.and.cards, deck == d))
}

overall.num.trials <- function() {
    NROW(trials)
}

num.trials <- function(dk) {
    NROW(subset(trials, trials$deck == dk))
}

perc.trials <- function(dk) {
    NROW(subset(trials, trials$deck == dk)) / NROW(trials)
}

overall.num.cards <- function() {
    NROW(cards)
}

num.cards <- function(dk) {
    NROW(subset(decks.and.cards, deck == dk))
}

perc.cards <- function(dk) {
    NROW(subset(decks.and.cards, deck == dk)) / NROW(cards)
}

overall.proportion.correct <- function(t) {
    NROW(subset(t, result)) / NROW(t)
}

proportion.correct <- function(dk) {
    NROW(subset(trials, result & trials$deck == dk)) / NROW(subset(trials, trials$deck == dk))
}

proportion.correct.before <- function(t) {
    overall.proportion.correct(subset(trials, time < t))
}

daily.accuracy <- function(t) {
    overall.proportion.correct(subset(trials, time < t & time >= (t - 86400000)))
}

#first.trials <- data.frame(card=cards, time=mapply(first.trial.time, cards))

daily.new.cards <- function(t) {
    NROW(subset(first.trials, time < t & time >= (t - 86400000)))
}

daily.trials <- function(t) {
    NROW(subset(trials, time < t & time >= (t - 86400000)))
}

card.success.time <- function(c) {
    first <- 0
    last <- 0
    # Note: assumes that rows are ordered by increasing time
    s <- subset(trials, card == c)
    for (i in 1:nrow(s)) {
        trial <- s[i,]
        time <- trial$time
    	if (trial$result) {
	    last <- time
	    if (first == 0) {
	        first <- time
	    }
	} else {
	    first <- 0
	    last <- 0
	}
    }
    last - first
}

card.life.time <- function(c) {
    s <- subset(trials, card == c)
    max(s$time) - min(s$time)
}

card.easiness <- function(c) {
    l <- card.life.time(c)
    if (l > 0) {
        card.success.time(c) / card.life.time(c)
    } else {
        0
    }
}

deck.average.easiness <- function(d) {
    s <- subset(decks.and.cards, deck == d)
    sum(mapply(card.easiness, s$card)) / num.cards(d)
}

deck.info <- function() {
    data.frame(
        name = decks,
        num.cards = mapply(num.cards, decks),
	perc.cards = mapply(perc.cards, decks),
        num.trials = mapply(num.trials, decks),
	perc.trials = mapply(perc.trials, decks),
        prop.correct = mapply(proportion.correct, decks),
	av.easiness = mapply(deck.average.easiness, decks),
	aced = mapply(deck.proportion.correct.on.first.try, decks))
}

################################################################################
# sandbox

card.success.time("hsk4_characters \\u6760")
card.life.time("hsk4_characters \\u6760")
card.relative.success("hsk4_characters \\u6760")
card.relative.success("french_vocabulary innovation")
deck.average.relative.success("french_vocabulary")


#########################################
# deck-specific metrics

i <- deck.info()

par(mar=c(5,10,3,3)) # increase y-axis margin.
barplot(i$av.easiness, las=2, horiz=TRUE, names.arg=i$name,
    sub = "Average easiness of cards")

# These give a similar (though not identical) order of decks by "easiness".
i[with(i, order(-i$av.easiness)),]
i[with(i, order(-i$prop.correct)),]
# They are in fact highly correlated (0.6291393)
cor(i$prop.correct, i$av.easiness)
# Proportion correct is even more highly correlated with "acing" (0.9254643)
cor(i$prop.correct, i$aced)
# Easiness and acing are not highly correlated (0.3946932)
cor(i$av.easiness, i$aced)


########################################

overall.num.trials()
overall.num.cards()
overall.proportion.correct(trials)

min(trials$time)
max(trials$time)

num.cards("french_vocabulary")
num.trials("french_vocabulary")
proportion.correct("french_vocabulary")


################################################################################
# plots

########################################
# age distribution

age <- max(last.trials$time) - last.trials$time

h <- hist(t(as.vector(age / (1000 * 60 * 60 * 24))), breaks=20)
barplot(h$counts,
    names.arg=paste(h$breaks[1:NROW(h$breaks)-1], "-" , h$breaks[2:NROW(h$breaks)]),
    xlab="# days since last trial", ylab="# of cards")


########################################
# overall

times <- seq(min(trials$time), max(trials$time), length.out = 1000)
plot(
    (times - min(trials$time)) / 86400000,
    mapply(proportion.correct.before, times),
    ylab="proportion of trials correct", xlab="time elapsed (days)", type="l")


########################################
# day by day

times <- seq(min(trials$time), max(trials$time), length.out = 1000)
plot(
    (times - min(trials$time)) / 86400000,
    mapply(daily.trials, times),
    ylab="# of trials in preceding day", xlab="time elapsed (days)", type="l")
plot(
    (times - min(trials$time)) / 86400000,
    mapply(daily.accuracy, times),
    ylab="accuracy in preceding day", xlab="time elapsed (days)", type="l")
plot(
    (times - min(trials$time)) / 86400000,
    mapply(daily.new.cards, times),
    ylab="# of new cards tried in preceding day", xlab="time elapsed (days)", type="l")

