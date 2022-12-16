# sbt-version-policy

Set proper versionPolicyIntention:

- ```scala
  // Your next release will provide no compatibility guarantees with the
  // previous one.
  ThisBuild / versionPolicyIntention := Compatibility.None
  ```
- ```scala
  // Your next release will be binary compatible with the previous one,
  // but it may not be source compatible.
  ThisBuild / versionPolicyIntention := Compatibility.BinaryCompatible
  ```
- ```scala
  // Your next release will be both binary compatible and source compatible
  // with the previous one.
  ThisBuild / versionPolicyIntention := Compatibility.BinaryAndSourceCompatible
  ```

Check that version is set correctly.

`sbt versionPolicyCheck`
`sbt versionCheck`

Then just `sbt publishSigned`

Go to `https://s01.oss.sonatype.org/`, select the project, click **Close** from the menu, and then **Release**.
