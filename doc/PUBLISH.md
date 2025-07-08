# Publishing

1. Set new version in build.sbt
2. Start sbt server - `sbt`.
3. Run `publishSigned` inside sbt console.
4. Run `sonaUpload` inside sbt console and then publish on 'https://central.sonatype.com/'. Or run `sonaRelease` to both upload and publish.
