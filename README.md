# Welcome to Semantic Synchrony!

## The experience | Why

Semantic Synchrony lets anyone edit and use a knowledge graph. That's what Google, Siri, Alexa and others use to answer questions about the world. Semantic Synchrony, however, is easy: There are only about 25 commands you need to know, and the entire [introductory video course](https://github.com/synchrony/smsn/wiki/A-video-introduction-to-Semantic-Synchrony) takes less than 45 minutes.

Some essays on the benefits of personal knowledge mapping have been collected at [smsn-why](https://github.com/synchrony/smsn-why).

## The software | How
Disclaimer (and good news!): Semantic Synchrony is [evolving](https://github.com/synchrony/smsn-why/blob/master/caution-unstable.md).

### Installing it
Semantic Synchrony is among the easier Docker applications to [install](https://github.com/synchrony/smsn/wiki/installation) -- just run the Docker container, add a few lines to your .emacs config file, start Emacs, and run the `smsn-mode` command. You don't need to be good at Emacs.

### Using it
This brief [howto](https://github.com/synchrony/smsn/wiki/What-you-need-to-know-to-use-Semantic-Synchrony) explains everything you will need to know to use Semantic Synchrony. It assumes no prior familiarity with Emacs, knowledge graphs, or any other technology. (For a brief list of critical commands, or the complete list, see the [smsn-mode wiki](https://github.com/synchrony/smsn-mode/wiki).

### Escaping it (is easy)
Fear not the commitment! Semantic Synchrony exports to many formats, including plain text, with one file corresponding to each note in the graph. Even shared authorship is escapable, because the whole history of changes can be kept in Git.

### Hacking it
Please see the [invitation to coders](https://github.com/synchrony/smsn-why/blob/master/invitations/to-coders.md).

## Running the Standalone Server

The SmSn server can run as a standalone Java application with file-based storage (SQLite + Lucene + .smsn files).

### Configuration

Create a `smsn.yaml` configuration file. The server looks for this file in the current working directory.

```yaml
# Index directory for SQLite and Lucene indices
indexDirectory: /path/to/index

# Activity log (optional)
activityLog: /path/to/activity.log

# Data sources - each source is a directory of .smsn atom files
sources:
- location: /path/to/public-notes
  name: public
  code: s
  color: 0x00e000
- location: /path/to/personal-notes
  name: personal
  code: r
  color: 0xffc000
- location: /path/to/private-notes
  name: private
  code: a
  color: 0xff0000
  
# Optional settings
verbose: false
thingNamespace: http://example.org/things/
```

Each source `location` should be a directory containing `.smsn` atom files (one per note). These directories can be Git repositories for version control.

### Scripts

Helper scripts are provided in `bin/`:

```bash
# Build the server (run from project root)
bin/build-standalone-server.sh

# Start the server (run from directory containing smsn.yaml)
cd /path/to/directory/with/smsn.yaml
/path/to/smsn/bin/start-standalone-server.sh

# Stop the server (run from anywhere)
/path/to/smsn/bin/stop-standalone-server.sh
```

The start script will automatically build if needed. The server starts on `0.0.0.0:8182` and accepts WebSocket connections at `/gremlin`.

### Cycling the Graph (Backup and Rebuild)

To rebuild the index from source files (useful after external edits to .smsn files or to recover from corruption):

1. Stop the server: `bin/stop-standalone-server.sh`
2. Delete or move the index directory
3. Start the server (it will rebuild the index automatically)

## Contact us through Github (here), [Gitter](https://gitter.im/synchrony/Lobby) or [Facebook](https://www.facebook.com/pg/semanticsynchrony/)

Join us! Let us grow (what|how we know about) the world for each other.
