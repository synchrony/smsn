package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

/**
 * The Timeline Ontology as a collection of terms
 * This class was created on 2013-10-16 from Timeline Ontology version 1.0 published 29th October 2007
 */
public interface Timeline {
    String NAMESPACE = "http://purl.org/NET/c4dm/timeline.owl#";

    // classes
    IRI
            AbstractInstant = RDF4JUtil.createIRI(NAMESPACE + "AbstractInstant"),
            AbstractInterval = RDF4JUtil.createIRI(NAMESPACE + "AbstractInterval"),
            AbstractTimeLine = RDF4JUtil.createIRI(NAMESPACE + "AbstractTimeLine"),
            ContinuousTimeLine = RDF4JUtil.createIRI(NAMESPACE + "ContinuousTimeLine"),
            DiscreteInstant = RDF4JUtil.createIRI(NAMESPACE + "DiscreteInstant"),
            DiscreteInterval = RDF4JUtil.createIRI(NAMESPACE + "DiscreteInterval"),
            DiscreteTimeLine = RDF4JUtil.createIRI(NAMESPACE + "DiscreteTimeLine"),
            Instant = RDF4JUtil.createIRI(NAMESPACE + "Instant"),
            Interval = RDF4JUtil.createIRI(NAMESPACE + "Interval"),
            OriginMap = RDF4JUtil.createIRI(NAMESPACE + "OriginMap"),
            PhysicalTimeLine = RDF4JUtil.createIRI(NAMESPACE + "PhysicalTimeLine"),
            RelativeInstant = RDF4JUtil.createIRI(NAMESPACE + "RelativeInstant"),
            RelativeInterval = RDF4JUtil.createIRI(NAMESPACE + "RelativeInterval"),
            RelativeTimeLine = RDF4JUtil.createIRI(NAMESPACE + "RelativeTimeLine"),
            ShiftMap = RDF4JUtil.createIRI(NAMESPACE + "ShiftMap"),
            TimeLine = RDF4JUtil.createIRI(NAMESPACE + "TimeLine"),
            TimeLineMap = RDF4JUtil.createIRI(NAMESPACE + "TimeLineMap"),
            UTInstant = RDF4JUtil.createIRI(NAMESPACE + "UTInstant"),
            UTInterval = RDF4JUtil.createIRI(NAMESPACE + "UTInterval"),
            UniformSamplingMap = RDF4JUtil.createIRI(NAMESPACE + "UniformSamplingMap"),
            UniformSamplingWindowingMap = RDF4JUtil.createIRI(NAMESPACE + "UniformSamplingWindowingMap"),
            UniformWindowingMap = RDF4JUtil.createIRI(NAMESPACE + "UniformWindowingMap");

    // properties
    IRI
            after = RDF4JUtil.createIRI(NAMESPACE + "after"),
            at = RDF4JUtil.createIRI(NAMESPACE + "at"),
            atDate = RDF4JUtil.createIRI(NAMESPACE + "atDate"),
            atDateTime = RDF4JUtil.createIRI(NAMESPACE + "atDateTime"),
            atDuration = RDF4JUtil.createIRI(NAMESPACE + "atDuration"),
            atInt = RDF4JUtil.createIRI(NAMESPACE + "atInt"),
            atReal = RDF4JUtil.createIRI(NAMESPACE + "atReal"),
            atYear = RDF4JUtil.createIRI(NAMESPACE + "atYear"),
            atYearMonth = RDF4JUtil.createIRI(NAMESPACE + "atYearMonth"),
            before = RDF4JUtil.createIRI(NAMESPACE + "before"),
            beginsAtDateTime = RDF4JUtil.createIRI(NAMESPACE + "beginsAtDateTime"),
            beginsAtDuration = RDF4JUtil.createIRI(NAMESPACE + "beginsAtDuration"),
            beginsAtInt = RDF4JUtil.createIRI(NAMESPACE + "beginsAtInt"),
            contains = RDF4JUtil.createIRI(NAMESPACE + "contains"),
            delay = RDF4JUtil.createIRI(NAMESPACE + "delay"),
            domainTimeLine = RDF4JUtil.createIRI(NAMESPACE + "domainTimeLine"),
            duration = RDF4JUtil.createIRI(NAMESPACE + "duration"),
            durationInt = RDF4JUtil.createIRI(NAMESPACE + "durationInt"),
            durationXSD = RDF4JUtil.createIRI(NAMESPACE + "durationXSD"),
            during = RDF4JUtil.createIRI(NAMESPACE + "during"),
            end = RDF4JUtil.createIRI(NAMESPACE + "end"),
            endsAtDateTime = RDF4JUtil.createIRI(NAMESPACE + "endsAtDateTime"),
            endsAtDuration = RDF4JUtil.createIRI(NAMESPACE + "endsAtDuration"),
            endsAtInt = RDF4JUtil.createIRI(NAMESPACE + "endsAtInt"),
            equals = RDF4JUtil.createIRI(NAMESPACE + "equals"),
            finishedBy = RDF4JUtil.createIRI(NAMESPACE + "finishedBy"),
            finishes = RDF4JUtil.createIRI(NAMESPACE + "finishes"),
            hopSize = RDF4JUtil.createIRI(NAMESPACE + "hopSize"),
            meets = RDF4JUtil.createIRI(NAMESPACE + "meets"),
            metBy = RDF4JUtil.createIRI(NAMESPACE + "metBy"),
            origin = RDF4JUtil.createIRI(NAMESPACE + "origin"),
            overlappedBy = RDF4JUtil.createIRI(NAMESPACE + "overlappedBy"),
            overlaps = RDF4JUtil.createIRI(NAMESPACE + "overlaps"),
            rangeTimeLine = RDF4JUtil.createIRI(NAMESPACE + "rangeTimeLine"),
            sampleRate = RDF4JUtil.createIRI(NAMESPACE + "sampleRate"),
            start = RDF4JUtil.createIRI(NAMESPACE + "start"),
            startedBy = RDF4JUtil.createIRI(NAMESPACE + "startedBy"),
            starts = RDF4JUtil.createIRI(NAMESPACE + "starts"),
            timeline = RDF4JUtil.createIRI(NAMESPACE + "timeline"),
            windowLength = RDF4JUtil.createIRI(NAMESPACE + "windowLength");

    // individuals
    IRI
            universaltimeline = RDF4JUtil.createIRI(NAMESPACE + "universaltimeline");
}
