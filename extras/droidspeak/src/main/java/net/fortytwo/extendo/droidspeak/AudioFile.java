package net.fortytwo.extendo.droidspeak;

import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import java.io.File;

// Note: although I've made the members public for simplicity, the methods rely on
//       outside access being read-only.  Also, beware of making changes, internally,
//       to an AudioFile when you do a conversion (as opposed to creating a new file
//       or AudioFile)
public class AudioFile {

    public String name;
    public File file;
    public AudioInputStream stream;
    public AudioFileFormat fileFormat;
    public AudioFormat audioFormat;
    public AudioFileFormat.Type fileType;
    public int frameSize;

    /* load from a file */
    public AudioFile(String name0) throws Exception {
        name = name0;
        file = new File(name);
        stream = AudioSystem.getAudioInputStream(file);
        audioFormat = stream.getFormat();
        fileFormat = AudioSystem.getAudioFileFormat(file);
        fileType = fileFormat.getType();
        frameSize = audioFormat.getFrameSize();
    }

    public AudioFile(AudioFile a, AudioFileFormat aff) throws Exception {

    }

    public AudioFile(AudioFile a, AudioFileFormat.Type at) throws Exception {

    }

    public AudioFile(AudioFile a, AudioFormat af) throws Exception {

    }

    /* WARNING: this will only work for AudioFiles which have been derived from a File */
    public boolean rewind() {
        try {
            stream = AudioSystem.getAudioInputStream(file);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /* Save with different audio format and different file type */
    public boolean saveAs(AudioFileFormat.Type fileType_out, AudioFormat audioFormat_out, String name_out) {
        try {
            if ((audioFormat_out == audioFormat) || AudioSystem.isConversionSupported(audioFormat_out, audioFormat)) {
                File file_out = new File(name_out);
                AudioInputStream ais_out = AudioSystem.getAudioInputStream(audioFormat_out, stream);
                if (AudioSystem.isFileTypeSupported(fileType_out, ais_out)) {
                    AudioSystem.write(ais_out, fileType_out, file_out);
                    return true;
                }
                System.out.println("1!");
            }
            System.out.println("2!");
            return false;
        } catch (Exception e) {
            System.out.println("3!");
            return false;
        }
    }

    /*
      Save as a different file type, but with same audio format
      As far as I know, it should be possible to convert any recognized audio file
      format to any other recognized format, so long as the AudioFormat is not converted
    */
    public boolean saveAs(AudioFileFormat.Type fileType_out, String name_out) {
        return saveAs(fileType_out, audioFormat, name_out);
    }

    /* Save with a different audio format, same file type */
    public boolean saveAs(AudioFormat audioFormat_out, String name_out) {
        return saveAs(fileType, audioFormat_out, name_out);
    }

    public void displayFileInfo() {
        System.out.println("Audio file format = " + fileFormat.toString());
        System.out.println("File type = " + fileType.toString());
        System.out.println("Bytes per frame = " + frameSize);
        System.out.println("Format supports mark() and reset(): " + String.valueOf(stream.markSupported()));
    }

}
