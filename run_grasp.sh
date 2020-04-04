#!/bin/bash
# /Users/sainatha/emacs-spacemacs-config/grasp/server/grasp_server.py --path /Users/sainatha/Dropbox/org/grasp.org --template $'* TODO [#H] [[%:link][%:description]]\n** Selection\n%:selection\n** Comment\n%:comment\n'

# Default template
# /Users/sainatha/emacs-spacemacs-config/grasp/server/grasp_server.py --path /Users/sainatha/Dropbox/org/grasp.org

# /Users/sainatha/emacs-spacemacs-config/grasp/server/grasp_server.py --path /Users/sainatha/Dropbox/org/grasp.org --template $'* TODO [#H] [[%:link][%:description]] %:tags \n** Selection\n%:selection\n** Comment\n%:comment\n'

/Users/sainatha/emacs-spacemacs-config/grasp/server/grasp_server.py --path /Users/sainatha/Dropbox/org/grasp.org --template $'* TODO [#H] [[%:link][%:description]] %:tags \n%:comment\n\n%:selection\n'
