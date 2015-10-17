package net.fortytwo.smsn.rdf.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * The Timeline Ontology as a collection of terms
 * This class was created on 2013-10-16 from Timeline Ontology version 1.0 published 29th October 2007
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Timeline {
    public static final String NAMESPACE = "http://purl.org/NET/c4dm/timeline.owl#";

    // classes
    public static final URI
            AbstractInstant = new URIImpl(NAMESPACE + "AbstractInstant"),
            AbstractInterval = new URIImpl(NAMESPACE + "AbstractInterval"),
            AbstractTimeLine = new URIImpl(NAMESPACE + "AbstractTimeLine"),
            ContinuousTimeLine = new URIImpl(NAMESPACE + "ContinuousTimeLine"),
            DiscreteInstant = new URIImpl(NAMESPACE + "DiscreteInstant"),
            DiscreteInterval = new URIImpl(NAMESPACE + "DiscreteInterval"),
            DiscreteTimeLine = new URIImpl(NAMESPACE + "DiscreteTimeLine"),
            Instant = new URIImpl(NAMESPACE + "Instant"),
            Interval = new URIImpl(NAMESPACE + "Interval"),
            OriginMap = new URIImpl(NAMESPACE + "OriginMap"),
            PhysicalTimeLine = new URIImpl(NAMESPACE + "PhysicalTimeLine"),
            RelativeInstant = new URIImpl(NAMESPACE + "RelativeInstant"),
            RelativeInterval = new URIImpl(NAMESPACE + "RelativeInterval"),
            RelativeTimeLine = new URIImpl(NAMESPACE + "RelativeTimeLine"),
            ShiftMap = new URIImpl(NAMESPACE + "ShiftMap"),
            TimeLine = new URIImpl(NAMESPACE + "TimeLine"),
            TimeLineMap = new URIImpl(NAMESPACE + "TimeLineMap"),
            UTInstant = new URIImpl(NAMESPACE + "UTInstant"),
            UTInterval = new URIImpl(NAMESPACE + "UTInterval"),
            UniformSamplingMap = new URIImpl(NAMESPACE + "UniformSamplingMap"),
            UniformSamplingWindowingMap = new URIImpl(NAMESPACE + "UniformSamplingWindowingMap"),
            UniformWindowingMap = new URIImpl(NAMESPACE + "UniformWindowingMap");

    // properties
    public static final URI
            after = new URIImpl(NAMESPACE + "after"),
            at = new URIImpl(NAMESPACE + "at"),
            atDate = new URIImpl(NAMESPACE + "atDate"),
            atDateTime = new URIImpl(NAMESPACE + "atDateTime"),
            atDuration = new URIImpl(NAMESPACE + "atDuration"),
            atInt = new URIImpl(NAMESPACE + "atInt"),
            atReal = new URIImpl(NAMESPACE + "atReal"),
            atYear = new URIImpl(NAMESPACE + "atYear"),
            atYearMonth = new URIImpl(NAMESPACE + "atYearMonth"),
            before = new URIImpl(NAMESPACE + "before"),
            beginsAtDateTime = new URIImpl(NAMESPACE + "beginsAtDateTime"),
            beginsAtDuration = new URIImpl(NAMESPACE + "beginsAtDuration"),
            beginsAtInt = new URIImpl(NAMESPACE + "beginsAtInt"),
            contains = new URIImpl(NAMESPACE + "contains"),
            delay = new URIImpl(NAMESPACE + "delay"),
            domainTimeLine = new URIImpl(NAMESPACE + "domainTimeLine"),
            duration = new URIImpl(NAMESPACE + "duration"),
            durationInt = new URIImpl(NAMESPACE + "durationInt"),
            durationXSD = new URIImpl(NAMESPACE + "durationXSD"),
            during = new URIImpl(NAMESPACE + "during"),
            end = new URIImpl(NAMESPACE + "end"),
            endsAtDateTime = new URIImpl(NAMESPACE + "endsAtDateTime"),
            endsAtDuration = new URIImpl(NAMESPACE + "endsAtDuration"),
            endsAtInt = new URIImpl(NAMESPACE + "endsAtInt"),
            equals = new URIImpl(NAMESPACE + "equals"),
            finishedBy = new URIImpl(NAMESPACE + "finishedBy"),
            finishes = new URIImpl(NAMESPACE + "finishes"),
            hopSize = new URIImpl(NAMESPACE + "hopSize"),
            meets = new URIImpl(NAMESPACE + "meets"),
            metBy = new URIImpl(NAMESPACE + "metBy"),
            origin = new URIImpl(NAMESPACE + "origin"),
            overlappedBy = new URIImpl(NAMESPACE + "overlappedBy"),
            overlaps = new URIImpl(NAMESPACE + "overlaps"),
            rangeTimeLine = new URIImpl(NAMESPACE + "rangeTimeLine"),
            sampleRate = new URIImpl(NAMESPACE + "sampleRate"),
            start = new URIImpl(NAMESPACE + "start"),
            startedBy = new URIImpl(NAMESPACE + "startedBy"),
            starts = new URIImpl(NAMESPACE + "starts"),
            timeline = new URIImpl(NAMESPACE + "timeline"),
            windowLength = new URIImpl(NAMESPACE + "windowLength");

    // individuals
    public static final URI
            universaltimeline = new URIImpl(NAMESPACE + "universaltimeline");
}
