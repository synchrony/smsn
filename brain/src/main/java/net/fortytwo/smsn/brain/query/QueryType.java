package net.fortytwo.smsn.brain.query;

/**
 * Types of keyword search queries supported by SmSn
 */
public enum QueryType {
    /** Full-text search on note titles */
    FullText,

    /** Search by acronym (first letters of words) */
    Acronym,

    /** Search by shortcut key */
    Shortcut,

    /** Ripple query (external integration) */
    Ripple
}
