"""Put the repo root on sys.path so tests import scripts.verification the same way the CLI does."""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
