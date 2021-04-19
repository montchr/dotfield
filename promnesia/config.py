from promnesia import Source

# 'auto' indexer tries its best at indexing plaintext stuff
# - plaintext like org-mode/markdown/HTML
# - structured formats like JSON and CSV
from promnesia.sources import auto

# 'guess' indexer can do even more in addition:
# - HTTP links (to index the contents of a website)
# - Github links (to index the contents of a git repository
from promnesia.sources import guess

'''
List of sources to use.

You can specify your own, add more sources, etc.
See https://github.com/karlicoss/promnesia#setup for more information
'''
SOURCES = [
    Source(
        auto.index,
        # just some arbitrary directory with plaintext files
        '/usr/include/c++/',
        '/usr/local/include/c++/', # on apple they are here apparenlty..
    )
]

''''
Optional setting.
A directory where promnesia.sqlite will be stored.
'''
OUTPUT_DIR = '~/.local/share/promnesia'
