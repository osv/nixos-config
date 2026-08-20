# Kopia backup

Kopia stores encrypted snapshots in Google Drive through rclone:

```text
Kopia -> rclone remote gdrive -> Google Drive/kopia/shared
```

The module installs Kopia, KopiaUI, and rclone. It also persists their
configuration and stores the repository password in the desktop keyring.

## Important data

Keep these two things available for disaster recovery:

- The Kopia repository password in `pass` at
  `backup/kopia-google-drive`.
- Access to the Google account containing `gdrive:kopia/shared`.

The repository password encrypts the backups. The Google OAuth token only gives
rclone access to Google Drive and is configured separately on every computer.

Never pass the repository password using `--password` or `KOPIA_PASSWORD`. Copy
it when needed with:

```bash
pass show -c backup/kopia-google-drive
```

Do not modify `gdrive:kopia/shared` manually and never run `rclone sync` against
it. Kopia must manage repository files itself.

## Initial setup on `krypton`

### Create a Google OAuth client

Do not use rclone's shared Google Drive client ID: it is being retired and will
stop working during 2026. Create a personal OAuth client first, following the
[official rclone instructions](https://rclone.org/drive/#making-your-own-client-id):

1. Open the [Google API Console](https://console.developers.google.com/) and
   create or select a project.
2. Enable **Google Drive API** under **APIs & Services**.
3. Configure the OAuth consent screen:
   - application name: `rclone`;
   - audience: **External** for a personal Google account, or **Internal** for
     users of the same Google Workspace organization;
   - add your Google account as a user.
4. Under **Data Access**, add only this scope:
   `https://www.googleapis.com/auth/drive.file`. It lets rclone manage files it
   creates without granting access to all existing files in Google Drive.
5. Create an OAuth client with application type **Desktop app**.
6. Save its **Client ID** and **Client secret** for the next step and disaster
   recovery. Use this same OAuth client on every workstation: `drive.file`
   access is associated with the application that created the files.
7. For an External app, publish it under **Audience**. A personal app does not
   need Google verification, but an app left in Testing mode issues grants that
   expire after seven days.

### Configure the rclone remote

Run:

```bash
rclone config
```

Use these answers (the option numbers may change between rclone versions):

1. Choose **New remote**.
2. Set the remote name to `gdrive`.
3. Choose **Google Drive** as the storage type.
4. Enter the personal OAuth **Client ID** and **Client secret** created above.
5. Choose **Access to files created by rclone only** (**drive.file**) for the
   scope. Do not choose full Drive access.
6. Leave `service_account_file` empty.
7. Skip advanced configuration.
8. If rclone warns that its shared client ID is being retired, choose **No** and
   enter the personal Client ID and Client secret. This prompt only appears if
   they were left empty earlier.
9. Allow rclone to open a browser (**Use web browser to automatically
   authenticate rclone with remote?**). Log in to the Google account that will
   hold the backup and approve access.
10. Choose **No** for Shared Drive unless the repository is intentionally stored
   in a Google Workspace Shared Drive.
11. Review the result and confirm it.

The resulting remote must be named `gdrive`, because Kopia is configured to use
`gdrive:kopia/shared`. Create the repository directory through rclone so it is
covered by the `drive.file` permission, then verify access:

```bash
rclone listremotes
rclone mkdir gdrive:kopia/shared
rclone lsd gdrive:kopia
```

With `drive.file`, rclone cannot browse unrelated existing Drive files. It can
manage the `kopia` directory and repository objects because it created them.
Setting `root_folder_id` in advanced configuration can make a folder appear as
the remote root, but it is only a path restriction, not an OAuth security
boundary, and is unnecessary here.

The OAuth token is stored in `~/.config/rclone/rclone.conf`. This module persists
that directory with mode `0700`; do not copy or commit `rclone.conf`. Authorize
each workstation separately, but use the same Client ID and Client secret.

Create the repository password once:

```bash
pass generate backup/kopia-google-drive 32
pass show -c backup/kopia-google-drive
```

In KopiaUI, create an **Rclone** repository:

- Remote Path: `gdrive:kopia/shared`
- Executable: `/run/current-system/sw/bin/rclone`
- Password: the value copied from `pass`

Add existing `~/Documents` and `~/Pictures` as separate snapshot sources. Keep
snapshots manual and configure retention as follows:

- 10 latest
- 7 daily
- 4 weekly
- 12 monthly

Keep `krypton` as the maintenance owner.

## Routine operation

Create snapshots from KopiaUI or the CLI:

```bash
kopia snapshot create ~/Documents
kopia snapshot create ~/Pictures
kopia snapshot list
```

Check repository and maintenance status periodically:

```bash
kopia repository status
kopia maintenance info
```

Run full maintenance only on `krypton`:

```bash
kopia maintenance run --full
```

Kopia should open the repository without asking for its password. It must not
create an unprotected password fallback file:

```bash
test ! -e ~/.config/kopia/repository.config.kopia-password
kopia repository status
```

If the fallback file exists, fix `pass-secret-service`, remove the fallback,
and reconnect the repository so Kopia saves the password in the keyring.

## Restore

Inspect snapshots and restore into a separate directory first:

```bash
kopia snapshot list
kopia snapshot show <snapshot-id>
mkdir -p /tmp/kopia-restore
kopia restore <snapshot-id> /tmp/kopia-restore
```

Verify the restored data before copying it over existing files. Perform a small
test restore periodically rather than waiting for an emergency.

## Add another workstation

1. Run `rclone config` using the same OAuth Client ID, Client secret, and
   `drive.file` scope; create a separate local `gdrive` authorization.
2. Verify access with `rclone lsd gdrive:kopia`.
3. Synchronize the normal `backup/kopia-google-drive` pass entry.
4. Connect KopiaUI to the existing Rclone repository at
   `gdrive:kopia/shared`.
5. Enter the password copied with
   `pass show -c backup/kopia-google-drive` once.
6. Add only source directories that exist on this workstation.

Snapshots are separated by `user@hostname:/path`. A source missing on one host
does not delete snapshots created on another host.

## Recovery on a clean system

1. Install this NixOS configuration and log in again.
2. Restore the GPG keys and password store containing
   `backup/kopia-google-drive`.
3. Recreate `gdrive` with the original OAuth Client ID, Client secret, and
   `drive.file` scope; verify it using `rclone lsd gdrive:kopia`.
4. Connect KopiaUI to the existing `gdrive:kopia/shared` repository.
5. Enter the repository password from `pass` once.
6. Run `kopia repository status` and restore data to a separate directory.
7. Recreate snapshot sources only after confirming that the restored data is
   correct.

After a reboot, verify that `rclone lsd gdrive:kopia`,
`kopia repository status`, and password lookup still work without
reconfiguration.
