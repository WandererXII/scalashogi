# Local publishing

Use this to test local scalashogi changes in lishogi before a release.

1. Publish a local snapshot from the scalashogi checkout:

    ```sh
    sbt 'set version := version.value + "-local-SNAPSHOT"; publishLocal'
    ```

    This appends `-local-SNAPSHOT` to the version in `build.sbt`. The full version is shown in the publish output; below it is referred to as `<local-version>`.

2. Start lishogi with the local version override:

    ```sh
    sbt -Dscalashogi.version=<local-version> run
    ```

    You can also add `-Dscalashogi.version=<local-version>` to lishogi's ignored `.sbtopts` file.

3. After changing scalashogi, republish and restart lishogi's sbt process to load the updated JAR.

4. Remove the override to return to lishogi's default published dependency.

Notes:

- This only publishes to the local Ivy repository; it does not upload anything.
- Lishogi code migrated to unreleased scalashogi APIs needs the local override until a matching release is available.
