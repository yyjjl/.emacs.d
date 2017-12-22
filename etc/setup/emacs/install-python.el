
(defvar python-extrenal-packages
  '("pylint"
    "pytest"
    "ipython"
    "jedi"
    "importmagic"
    "autopep8"
    "yapf"
    "flake8"
    "jupyter-console"))

(apply #'run-command "pip3" "install" "--user" python-extrenal-packages)
