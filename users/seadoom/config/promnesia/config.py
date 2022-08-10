from promnesia.common import Source

# https://github.com/karlicoss/HPI/blob/master/doc/SOURCES.org

# 'auto' indexer tries its best at indexing plaintext stuff
# - plaintext like org-mode/markdown/HTML
# - structured formats like JSON and CSV
from promnesia.sources import auto

# 'guess' indexer can do even more in addition:
# - HTTP links (to index the contents of a website)
# - Github links (to index the contents of a git repository
from promnesia.sources import guess

from promnesia.sources import (
    # fbmessenger,
    # rss,
    # signal,
    # takeout,
    twitter,
)

"""
List of sources to use.

You can specify your own, add more sources, etc.
See https://github.com/karlicoss/promnesia#setup for more information
"""
SOURCES = [
    Source(
        auto.index,
        "~/Documents/notes/",
        name="notes",
    ),
    # Source(fbmessenger),
    # Source(rss),
    # Source(signal),
    # Source(takeout),
    Source(lambda: twitter.index()),
]

OUTPUT_DIR = "~/.local/share/promnesia"

CACHE_DIR = "~/.cache/promnesia"
