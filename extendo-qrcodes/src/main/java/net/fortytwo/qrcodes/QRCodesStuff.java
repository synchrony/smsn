package net.fortytwo.qrcodes;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.WriterException;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;
import net.fortytwo.extendo.ExtendoBrain;
import net.fortytwo.extendo.ExtendoBrain;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.Hashtable;

/**
 * A generator of QR codes for MyOtherBrain identifiers.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class QRCodesStuff {
    public enum Style {
        /**
         * Generates compact (Version 1) but not very application-friendly QR codes
         * of the format "MOB 1234567"
         */
        VERSION1_COMPACT,

        /**
         * Generates human-friendly, but slightly less compact (Version 2) QR codes
         * of the format "MyOtherBrain 1234567"
         */
        VERSION2_FRIENDLY,

        /**
         * Generates relatively verbose (Version 3) QR codes which embed Semantic Web -compatible URIs
         * of the format "http://myotherbrain.fortytwo.net/things/1234567"
         */
        VERSION3_SEMANTICWEB
    }

    private final QRCodeWriter writer;
    private final Style style;
    private final Hashtable<EncodeHintType, Object> hints;

    public QRCodesStuff(final Style style) {
        writer = new QRCodeWriter();
        this.style = style;
        hints = new Hashtable<EncodeHintType, Object>();
        //hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
    }

    /**
     * @param key a seven-digit, base-64 encoded MyOtherBrain identifier
     * @return the QR code image
     * @throws com.google.zxing.WriterException
     *          if writing fails
     */
    public Image toQRCode(final String key) throws WriterException {
        String code;
        switch (style) {
            case VERSION1_COMPACT:
                code = "MOB" + key;
                break;
            case VERSION2_FRIENDLY:
                code = "MyOtherBrain " + key;
                break;
            case VERSION3_SEMANTICWEB:
                code = "http://myotherbrain.fortytwo.net/things/" + key;
                break;
            default:
                throw new IllegalStateException();
        }

        System.out.println(code);
        return urlToQRCode(code, 200, 200);
    }

    private BufferedImage urlToQRCode(final String url,
                                      final int width,
                                      final int height) throws WriterException {
        BitMatrix m = writer.encode(url, BarcodeFormat.QR_CODE, width, height, hints);
        return toImage(m);
    }

    // See: http://stackoverflow.com/questions/2489048/qr-code-encoding-and-decoding-using-zxing
    private static BufferedImage toImage(final BitMatrix matrix) {
        int width = matrix.getWidth();
        int height = matrix.getHeight();

        //create buffered image to draw to
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        //iterate through the matrix and draw the pixels to the image
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int grayValue = (matrix.get(y, x) ? 1 : 0) & 0xff;
                image.setRGB(x, y, (grayValue == 0 ? 0 : 0xFFFFFF));
            }
        }

        return image;
    }

    private static void showImage(final Image image) {
        JPanel panel = new JPanel() {
            //override paint method of panel
            public void paint(final Graphics g) {
                //draw the image
                if (image != null) {
                    g.drawImage(image, 0, 0, this);
                }
            }

        };

        JFrame f = new JFrame();

        f.getContentPane().add(panel);

        int width = image.getWidth(null);
        int height = image.getHeight(null);

        //show frame
        f.setBounds(0, 0, width, height);
        f.setVisible(true);
    }

    public static void main(final String[] args) throws Exception {
        int rows = 1;// 11;
        int cols = 1;// 8;

        QRCodesStuff stuff = new QRCodesStuff(Style.VERSION1_COMPACT);

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                String key = ExtendoBrain.createRandomKey();
                Image img = stuff.toQRCode(key);
                showImage(img);
            }
        }
    }
}
