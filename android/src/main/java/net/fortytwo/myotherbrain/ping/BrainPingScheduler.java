package net.fortytwo.myotherbrain.ping;

import android.content.SharedPreferences;
import android.util.Log;

import java.util.Arrays;
import java.util.Calendar;
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

    private static final boolean DEFAULT_ENABLED = false;
    private static final String DEFAULT_FREQUENCY = "1";
    private static final String DEFAULT_START_TIME = "10:00";
    private static final String DEFAULT_END_TIME = "22:00";

    private final Random random = new Random();
    private final SharedPreferences prefs;
    private final Runnable task;
    private Timer timer;

    private boolean enabled = true;
    private int frequency = 0;
    private long startTime = -1;
    private long endTime = -1;

    private Calendar[] schedule;
    private int scheduleIndex;

    // FIXME: this is a hack
    private static BrainPingScheduler INSTANCE;

    public static BrainPingScheduler getInstance() {
        return INSTANCE;
    }

    public BrainPingScheduler(final SharedPreferences prefs,
                              final Runnable task) {
        INSTANCE = this;

        this.prefs = prefs;
        this.task = task;
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

            Log.i("info", "brain ping preferences updated");
            scheduleNextPing(true);
        }
    }

    public void accept() {
        Log.i("info", "brain ping accepted");
        scheduleNextPing(false);
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

    private void createSchedule(final Calendar now) {
        long n = millisToday(now);
        Calendar morning = (Calendar) now.clone();
        morning.set(Calendar.HOUR_OF_DAY, 0);
        morning.set(Calendar.MINUTE, 0);
        morning.set(Calendar.MILLISECOND, 0);
        if (n >= endTime) {
            morning.add(Calendar.DAY_OF_YEAR, 1);
        }

        if (frequency < 1) {
            throw new IllegalStateException();
        }

        schedule = new Calendar[frequency];
        long span = endTime - startTime;

        for (int i = 0; i < frequency; i++) {
            long offset = startTime + (long) (random.nextDouble() * span);
            Calendar c = (Calendar) morning.clone();
            c.setTimeInMillis(c.getTimeInMillis() + offset);
            schedule[i] = c;
        }

        Arrays.sort(schedule);
    }

    private Calendar nextPingTime(final Calendar now,
                                  final boolean refresh) {
        if (refresh) {
            createSchedule(now);
            //...
        }

        // Break out when a suitable time is found.
        while (true) {
            scheduleIndex++;

            if (null == schedule || scheduleIndex >= schedule.length) {
                createSchedule(now);
                scheduleIndex = 0;
            }

            Calendar cal = schedule[scheduleIndex];
            if (cal.compareTo(now) > 0) {
                return cal;
            }
        }
    }

    private void scheduleNextPing(final boolean refreshSchedule) {
        timer.cancel();
        timer = new Timer();

        if (enabled) {
            Calendar now = new GregorianCalendar();
            Calendar next = nextPingTime(now, refreshSchedule);

            Log.i("info", "scheduling next ping for " + next);

            long delay = next.getTimeInMillis() - now.getTimeInMillis();

            timer.schedule(new TimerTask() {
                @Override
                public void run() {
                    task.run();
                }
            }, delay);
        }
    }
}
