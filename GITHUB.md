# Create the GitHub repository

Repository name to use: `saspy-learner`

Public URL it will live at: `https://github.com/<your-username>/saspy-learner`

Do the one-time machine setup first, then create an empty GitHub repo, then push this folder. Creating the repo empty avoids a merge on the first push.

## 1. Accounts and tools

1. Sign in at https://github.com. Create an account if you do not have one.
2. Install Git: https://git-scm.com/downloads
3. Install R 4.1+ and, if you want the Run App button, RStudio.
4. In a terminal, tell Git who you are. Use the email on your GitHub account.

```bash
git config --global user.name "Your Name"
git config --global user.email "you@example.com"
git config --global init.defaultBranch main
```

5. Authenticate. GitHub no longer accepts account passwords for Git.

- Easiest: install [GitHub CLI](https://cli.github.com/) and run `gh auth login`.
- Or create a personal access token (Settings → Developer settings → Personal access tokens) with `repo` scope, and use that token as the password when Git asks.

## 2. Create the empty repository on GitHub

1. Open https://github.com/new
2. Repository name: `saspy-learner`
3. Description: `Interactive SAS ↔ Python learner studio (R Shiny guide and pattern converter)`
4. Public is right for a learner guide. Choose Private only if the code must stay internal.
5. Do **not** tick “Add a README file”.
6. Do **not** add a .gitignore or licence here. This project already has both.
7. Click **Create repository**.
8. Leave the page open. You need the URL: `https://github.com/<your-username>/saspy-learner.git`

Topics worth adding after the first push (About → gear icon): `r`, `shiny`, `sas`, `pandas`, `python`, `learning`.

## 3. Put this project on your machine

If you downloaded the zip, unzip it so the folder contains `app.R` directly, not an extra nested folder.

```text
saspy-learner/
  app.R
  README.md
  R/
  www/
  examples/
```

Open a terminal in that folder.

## 4. First commit and push

```bash
cd saspy-learner
git init
git add .
git status
git commit -m "Initial SASPy Learner studio"
git branch -M main
git remote add origin https://github.com/<your-username>/saspy-learner.git
git push -u origin main
```

`git status` before the commit should list `app.R`, `R/`, `www/`, `examples/`, `README.md`, `LICENSE`, `.gitignore`. It should not list `.Rhistory` or `.Rproj.user`.

If Git asks for credentials, sign in with `gh auth login` or use a personal access token as the password. The username is your GitHub username, not your email.

Refresh the GitHub page. The README should render.

## 5. Confirm the app still runs from the clone

On another folder, or after a fresh clone:

```bash
git clone https://github.com/<your-username>/saspy-learner.git
cd saspy-learner
```

In R:

```r
install.packages(c("shiny", "bslib"))
shiny::runApp()
```

## 6. Everyday change loop

```bash
git checkout -b guide-dates
# edit files
git add R/content.R
git commit -m "Clarify SAS date versus pandas timestamp"
git push -u origin guide-dates
```

Open a pull request on GitHub from `guide-dates` into `main`. Merge it there. Then locally:

```bash
git checkout main
git pull
```

Small commits with a reason in the message are easier to undo than one dump of every edit.

## 7. If the first push is rejected

That means the GitHub repo was not empty (a README was generated). Either delete the repo and recreate it empty, or:

```bash
git pull origin main --rebase --allow-unrelated-histories
git push -u origin main
```

## 8. Web upload, if you cannot use Git yet

On the empty repository page, choose **uploading an existing file**, drag the contents of `saspy-learner` (not the parent folder), and commit. This is fine for the first publish. Switch to Git before you start editing, or GitHub and your laptop will diverge.

## 9. Optional: run it on the web

GitHub will not execute this app. shinyapps.io will.

1. Create a free account at https://www.shinyapps.io/
2. Copy the token snippet from the dashboard.
3. From the project folder in R:

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp()
```

The app name can also be `saspy-learner`.
