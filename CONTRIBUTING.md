### CONTRIBUTING.md

# Contributing

## How to Contribute

### Pull Requests

1. **Fork the Repository**: Click the "Fork" button at the top of this repository to create a personal copy.
2. **Clone the Fork**: Clone your fork to your local machine
   - Replace `{{ cookiecutter.github_username }}` with your GitHub username in the following command:
     ```bash
     git clone https://github.com/{{ cookiecutter.github_username }}/{{ cookiecutter.repo_name }}.git
     ```
3. **Create a Branch**: Create a new branch for your changes:
    - ```bash
        git checkout -b feature/your-feature-name
        ```
4. **Make Changes**: Make your changes to the codebase.
    - ```bash
        git add .
        git commit -m "Add your commit message here"
        ```
5. **Push Changes**: Push your changes to your fork:
    - ```bash
        git push origin feature/your-feature-name
        ```
6. **Create a Pull Request**: Go to the original repository and create a pull request from your fork.

### Code Style

Make sure to use pre-commit hooks to format your code before committing.
#### Installing pre-commit

1. Install `pre-commit` using pip:
    ```bash
    pip install pre-commit
    ```

2. Install the pre-commit hooks:
    ```bash
    pre-commit install
    ```

3. Run the pre-commit hooks manually on all files (optional but recommended):
    ```bash
    tox -e pre-commit
    ```