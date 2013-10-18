package net.fortytwo.extendo.ping;

import android.content.SharedPreferences;
import android.util.Log;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;

/**
 * User: josh
 * Date: 1/12/11
 * Time: 2:09 PM
 */
public class BrainPingScheduler {
    public static final String
            ENABLED = "brainping_enabled",
            START_TIME = "brainping_starttime",
            END_TIME = "brainping_endtime",
            FREQUENCY = "brainping_frequency";

    private static final String LOG_TAG = BrainPingScheduler.class.getName();

    private static final long MILLIS_IN_DAY = 24 * 60 * 60 * 1000;

    // Randomized delays will be within this ratio of the precise value.
    private static final double delayImprecision = 0.5;

    private static final boolean DEFAULT_ENABLED = false;
    private static final String DEFAULT_FREQUENCY = "1";
    private static final String DEFAULT_START_TIME = "10:00";
    private static final String DEFAULT_END_TIME = "22:00";

    private final Random random = new Random();
    private final SharedPreferences prefs;
    private final Runnable task;
    private final Collection<Pinger> pingers;

    private Timer timer;

    private boolean enabled = true;
    private int frequency = 0;
    private long startTime = -1;
    private long endTime = -1;

    // FIXME: this is a hack
    private static BrainPingScheduler INSTANCE;

    public static BrainPingScheduler getInstance() {
        return INSTANCE;
    }

    public BrainPingScheduler(final SharedPreferences prefs,
                              final Runnable task,
                              final Collection<Pinger> pingers) {
        INSTANCE = this;

        this.prefs = prefs;
        this.task = task;
        this.pingers = pingers;
        this.timer = new Timer();

        preferencesUpdated();
    }

    public void finalize() throws Throwable {
        timer.cancel();

        super.finalize();
    }

    public void preferencesUpdated() {
        boolean enabledTmp = prefs.getBoolean(ENABLED, DEFAULT_ENABLED);
        int frequencyTmp = Integer.valueOf(prefs.getString(FREQUENCY, DEFAULT_FREQUENCY));
        long startTimeTmp = parseTime(prefs.getString(START_TIME, DEFAULT_START_TIME));
        long endTimeTmp = parseTime(prefs.getString(END_TIME, DEFAULT_END_TIME));

        if (enabledTmp != enabled
                || frequencyTmp != frequency
                || startTimeTmp != startTime
                || endTimeTmp != endTime) {
            enabled = enabledTmp;
            frequency = frequencyTmp;
            startTime = startTimeTmp;
            endTime = endTimeTmp;

            Log.i(LOG_TAG, "brain ping preferences updated");
// TODO: restore me
//            schedulePings();
        }
    }

    private long parseTime(final String time) {
        int i = time.indexOf(":");
        String hours = time.substring(0, i);
        String minutes = time.substring(i + 1);
        if (hours.startsWith("0")) {
            hours = hours.substring(1);
        }
        if (minutes.startsWith("0")) {
            minutes = minutes.substring(1);
        }

        return millisToday(Integer.valueOf(minutes), Integer.valueOf(hours));
    }

    private long millisToday(final Calendar cal) {
        return millisToday(cal.get(Calendar.HOUR_OF_DAY), cal.get(Calendar.MINUTE));
    }

    private long millisToday(final int hours,
                             final int minutes) {
        return 1000 * 60 * (minutes + 60 * hours);
    }

    private long randomizeDelay(final long delay) {
        long d = (long) (delayImprecision * delay * (random.nextDouble() * 2 - 1));
        return delay + d;
    }

 /*
    private void schedulePings() {
        timer.cancel();
        timer = new Timer();

        if (enabled) {
            Calendar now = new GregorianCalendar();
            for (Pinger p : pingers) {
                scheduleNextPing(p, now);
            }


            long delay = next.getTimeInMillis() - now.getTimeInMillis();

            timer.schedule(new TimerTask() {
                @Override
                public void run() {
                    task.run();
                }
            }, delay);
        }
    }

    private void scheduleNextPing(final Pinger p,
                                  final Calendar now) {
        long millisPerDay = endTime - startTime;

        Calendar thisMorning = (Calendar) now.clone();
        thisMorning.set(Calendar.HOUR_OF_DAY, 0);
        thisMorning.set(Calendar.MINUTE, 0);
        thisMorning.set(Calendar.SECOND, 0);
        thisMorning.set(Calendar.MILLISECOND, 0);

        long f = startTime;
        long l = MILLIS_IN_DAY - endTime;

        long last = prefs.getLong(p.getName() + "_last_ping", now.getTimeInMillis());
        long rel = thisMorning.getTimeInMillis() - last;
        long days = rel / MILLIS_IN_DAY;
        long rem = rel % MILLIS_IN_DAY;

        long elapsed = rel - (days * gap)

        long lastRel = last - now.getTimeInMillis();

        //long rel = (long) (millisPerDay / p.getFrequency());
        //rel = randomizeDelay(rel);


        Log.i(LOG_TAG, "scheduling next '" + p.getName() + "' ping for " + next);
    }

    private long fromRelativeTime(final long rel,
                                  final Calendar now) {

    }

    private long toRelativeTime(final long abs,
                                final Calendar now) {
        Calendar thisMorning = (Calendar) now.clone();
        thisMorning.set(Calendar.HOUR_OF_DAY, 0);
        thisMorning.set(Calendar.MINUTE, 0);
        thisMorning.set(Calendar.SECOND, 0);
        thisMorning.set(Calendar.MILLISECOND, 0);

        //Calendar then = new GregorianCalendar();
        //then.setTime(new Date(abs));
        //int yearDiff = now.get(Calendar.YEAR) - then.get(Calendar.YEAR);
        //int dayDiff = now.

        long rel = now.getTimeInMillis() - abs;
        long days = rel / MILLIS_IN_DAY;

        Calendar then = new GregorianCalendar();
        then.setTime(new Date(abs));


    }
    */
}
