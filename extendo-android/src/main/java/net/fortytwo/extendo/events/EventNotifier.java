package net.fortytwo.extendo.events;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventNotifier {
    private static final int CAPACITY = 100;

    private final List<Event> events;

    private static final EventNotifier instance = new EventNotifier();

    public static EventNotifier getInstance() {
        return instance;
    }

    private EventNotifier() {
        events = new LinkedList<Event>();
    }

    public synchronized void addEvent(final String description) {
        if (events.size() == CAPACITY) {
            events.remove(0);
        }

        Event e = new Event(System.currentTimeMillis(), description);
        events.add(e);
    }

    public synchronized void clear() {
        events.clear();
    }

    public String getInfo() {
        StringBuilder sb = new StringBuilder();
        sb.append("<ul>/n");
        for (Event e : events) {
            sb.append("<li>").append(e.time).append(": ").append(e.description).append("</li>\n");
        }
        sb.append("</ul>\n");

        return sb.toString();
    }

    private static class Event {
        private final long time;
        private final String description;

        private Event(final long time,
                      final String description) {
            this.time = time;
            this.description = description;
        }
    }
}
