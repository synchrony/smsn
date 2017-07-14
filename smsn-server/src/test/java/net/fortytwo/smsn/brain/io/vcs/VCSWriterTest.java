package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class VCSWriterTest extends BrainTestBase {
    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void formatIsCorrect() {
        List<Format> formats = new VCSWriter().getFormats();
        assertEquals(1, formats.size());
        assertEquals("VCS", formats.get(0).getName());
    }

    @Test
    public void emptyGraphProducesEmptyDirectory() throws Exception {
        File dir = doExport();
        assertEquals(4, dir.listFiles().length);
        for (File subdir : dir.listFiles()) {
            assertTrue(subdir.isDirectory());
            assertEquals(0, subdir.listFiles().length);
        }
    }

    private TreeNode<Link> createTree() {
        TreeNode<Link> node = new TreeNodeDTO<>();
        Link link = new LinkDTO();
        link.setPage(new PageDTO());
        node.setValue(link);
        return node;
    }

    @Test
    public void singlePageIsWrittenCorrectly() throws Exception {
        String source = DefaultSources.PUBLIC;

        Atom root = createAtom("11111", "change me");
        root.setSource(source);

        TreeNode<Link> tree = createTreeDTO(root.getId(), "Arthur Dent");
        Page page = PageDTO.createTransitional();
        page.setSource(source);
        page.setContent(tree);
        page.setText("He's a jerk.\nA complete kneebiter.");
        tree.getValue().setPage(page);
        TreeNode<Link> fordTree = createTreeDTO(fordTopic.getId(), "Ford Prefect");
        fordTree.getValue().setPage(page);
        TreeNode<Link> zaphodTree = createTreeDTO(zaphodTopic.getId(), "Zaphod Beeblebrox");
        zaphodTree.getValue().setPage(page);
        tree.addChild(fordTree);
        tree.addChild(zaphodTree);

        queries.update(tree, Integer.MAX_VALUE, Filter.noFilter(), ViewStyle.Basic.Forward.getStyle());

        Atom ad = topicGraph.getAtomById(root.getId()).get();
        assertEquals(DefaultSources.PUBLIC, ad.getSource());
        assertEquals("Arthur Dent", ad.getTitle());
        assertEquals("He's a jerk.\nA complete kneebiter.", ad.getText());
        assertEquals(2, ListNode.toJavaList(ad.getChildren()).size());
        Atom random = ad.getChildren().getFirst();
        assertEquals("Ford Prefect", random.getTitle());

        File dir = doExport();
        assertEquals(4, dir.listFiles().length);
        File publicDir = new File(dir, "public");
        assertTrue(publicDir.exists() && publicDir.isDirectory());
        File arthurFile = new File(publicDir, root.getId());
        assertTrue(arthurFile.exists());
        List<String> lines = readLines(arthurFile);
        assertEquals(9, lines.size());
        assertEquals("@id " + root.getId(), lines.get(0));
        assertEquals("@title Arthur Dent", lines.get(1));
        assertEquals("created", readPropertyLine(lines.get(2)).getKey());
        assertEquals("@weight 0.5", lines.get(3));
        assertEquals("* :" + fordTopic.getId() + ": ", lines.get(4));
        assertEquals("* :" + zaphodTopic.getId() + ": ", lines.get(5));
        assertEquals("", lines.get(6));
        assertEquals("He's a jerk.", lines.get(7));
    }

    private Map.Entry<String, String> readPropertyLine(final String line) {
        Map<String, String> map = new HashMap<>();
        int atIndex = line.indexOf('@');
        int spaceIndex = line.indexOf(' ');
        String key = line.substring(atIndex+1, spaceIndex);
        String value = line.substring(spaceIndex+1);
        map.put(key, value);
        return map.entrySet().iterator().next();
    }

    private List<String> readLines(final File file) throws IOException {
        List<String> lines = new LinkedList<>();
        try (InputStream in = new FileInputStream(file)) {
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            String line;
            while (null != (line = br.readLine())) {
                lines.add(line);
            }
        }
        return lines;
    }

    private File doExport() throws IOException {
        VCSWriter writer = new VCSWriter();

        File dir = createVCSTestDirectory();
        BrainWriter.Context context = new BrainWriter.Context();
        context.setDestDirectory(dir);
        context.setTopicGraph(topicGraph);

        writer.doExport(context);
        return dir;
    }
}
