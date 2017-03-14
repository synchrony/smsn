package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * The Timeline Ontology as a collection of terms
 * This class was created on 2013-10-16 from Timeline Ontology version 1.0 published 29th October 2007
 */
public interface Timeline {
    String NAMESPACE = "http://purl.org/NET/c4dm/timeline.owl#";

    // classes
    IRI
            AbstractInstant = RDFAgents.createIRI(NAMESPACE + "AbstractInstant"),
            AbstractInterval = RDFAgents.createIRI(NAMESPACE + "AbstractInterval"),
            AbstractTimeLine = RDFAgents.createIRI(NAMESPACE + "AbstractTimeLine"),
            ContinuousTimeLine = RDFAgents.createIRI(NAMESPACE + "ContinuousTimeLine"),
            DiscreteInstant = RDFAgents.createIRI(NAMESPACE + "DiscreteInstant"),
            DiscreteInterval = RDFAgents.createIRI(NAMESPACE + "DiscreteInterval"),
            DiscreteTimeLine = RDFAgents.createIRI(NAMESPACE + "DiscreteTimeLine"),
            Instant = RDFAgents.createIRI(NAMESPACE + "Instant"),
            Interval = RDFAgents.createIRI(NAMESPACE + "Interval"),
            OriginMap = RDFAgents.createIRI(NAMESPACE + "OriginMap"),
            PhysicalTimeLine = RDFAgents.createIRI(NAMESPACE + "PhysicalTimeLine"),
            RelativeInstant = RDFAgents.createIRI(NAMESPACE + "RelativeInstant"),
            RelativeInterval = RDFAgents.createIRI(NAMESPACE + "RelativeInterval"),
            RelativeTimeLine = RDFAgents.createIRI(NAMESPACE + "RelativeTimeLine"),
            ShiftMap = RDFAgents.createIRI(NAMESPACE + "ShiftMap"),
            TimeLine = RDFAgents.createIRI(NAMESPACE + "TimeLine"),
            TimeLineMap = RDFAgents.createIRI(NAMESPACE + "TimeLineMap"),
            UTInstant = RDFAgents.createIRI(NAMESPACE + "UTInstant"),
            UTInterval = RDFAgents.createIRI(NAMESPACE + "UTInterval"),
            UniformSamplingMap = RDFAgents.createIRI(NAMESPACE + "UniformSamplingMap"),
            UniformSamplingWindowingMap = RDFAgents.createIRI(NAMESPACE + "UniformSamplingWindowingMap"),
            UniformWindowingMap = RDFAgents.createIRI(NAMESPACE + "UniformWindowingMap");

    // properties
    IRI
            after = RDFAgents.createIRI(NAMESPACE + "after"),
            at = RDFAgents.createIRI(NAMESPACE + "at"),
            atDate = RDFAgents.createIRI(NAMESPACE + "atDate"),
            atDateTime = RDFAgents.createIRI(NAMESPACE + "atDateTime"),
            atDuration = RDFAgents.createIRI(NAMESPACE + "atDuration"),
            atInt = RDFAgents.createIRI(NAMESPACE + "atInt"),
            atReal = RDFAgents.createIRI(NAMESPACE + "atReal"),
            atYear = RDFAgents.createIRI(NAMESPACE + "atYear"),
            atYearMonth = RDFAgents.createIRI(NAMESPACE + "atYearMonth"),
            before = RDFAgents.createIRI(NAMESPACE + "before"),
            beginsAtDateTime = RDFAgents.createIRI(NAMESPACE + "beginsAtDateTime"),
            beginsAtDuration = RDFAgents.createIRI(NAMESPACE + "beginsAtDuration"),
            beginsAtInt = RDFAgents.createIRI(NAMESPACE + "beginsAtInt"),
            contains = RDFAgents.createIRI(NAMESPACE + "contains"),
            delay = RDFAgents.createIRI(NAMESPACE + "delay"),
            domainTimeLine = RDFAgents.createIRI(NAMESPACE + "domainTimeLine"),
            duration = RDFAgents.createIRI(NAMESPACE + "duration"),
            durationInt = RDFAgents.createIRI(NAMESPACE + "durationInt"),
            durationXSD = RDFAgents.createIRI(NAMESPACE + "durationXSD"),
            during = RDFAgents.createIRI(NAMESPACE + "during"),
            end = RDFAgents.createIRI(NAMESPACE + "end"),
            endsAtDateTime = RDFAgents.createIRI(NAMESPACE + "endsAtDateTime"),
            endsAtDuration = RDFAgents.createIRI(NAMESPACE + "endsAtDuration"),
            endsAtInt = RDFAgents.createIRI(NAMESPACE + "endsAtInt"),
            equals = RDFAgents.createIRI(NAMESPACE + "equals"),
            finishedBy = RDFAgents.createIRI(NAMESPACE + "finishedBy"),
            finishes = RDFAgents.createIRI(NAMESPACE + "finishes"),
            hopSize = RDFAgents.createIRI(NAMESPACE + "hopSize"),
            meets = RDFAgents.createIRI(NAMESPACE + "meets"),
            metBy = RDFAgents.createIRI(NAMESPACE + "metBy"),
            origin = RDFAgents.createIRI(NAMESPACE + "origin"),
            overlappedBy = RDFAgents.createIRI(NAMESPACE + "overlappedBy"),
            overlaps = RDFAgents.createIRI(NAMESPACE + "overlaps"),
            rangeTimeLine = RDFAgents.createIRI(NAMESPACE + "rangeTimeLine"),
            sampleRate = RDFAgents.createIRI(NAMESPACE + "sampleRate"),
            start = RDFAgents.createIRI(NAMESPACE + "start"),
            startedBy = RDFAgents.createIRI(NAMESPACE + "startedBy"),
            starts = RDFAgents.createIRI(NAMESPACE + "starts"),
            timeline = RDFAgents.createIRI(NAMESPACE + "timeline"),
            windowLength = RDFAgents.createIRI(NAMESPACE + "windowLength");

    // individuals
    IRI
            universaltimeline = RDFAgents.createIRI(NAMESPACE + "universaltimeline");
}
