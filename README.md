Prismic Backup
==============

Unofficial CLI tool to backup a [Prismic.io](https://prismic.io/) repository.


## Why?

[Prismic.io](https://prismic.io/) currently has no backup or export feature, and it does not seem to be a priority.
See https://qa.prismic.io/102/is-there-a-way-to-export-all-data-and-site-content


## Features

- backup documents from a public [Prismic.io](https://prismic.io/) repository
- create a JSON file for each document
- backup several document types at once


## How to use

Run `stack exec prismic-backup -- [PARAMS]` (or `./prismic-backup [PARAMS]` if you only have a binary).

### Parameters description

- `endpoint`: *(string)* API endpoint of the repository. For example: `https://lesbonneschoses.prismic.io/api/documents/search` (the `/documents/search` part is necessary)
- `ref`: *(string)* reference to use
- `docTypes`: *(string)* a comma-separated list of document types
- `output`: *(optional path, default = "./output")* path to the backup directory (will be created if it does not exist)

Example:
```
stack exec prismic-backup -- --endpoint "https://lesbonneschoses.prismic.io/api/documents/search" --ref "UlfoxUnM08QWYXdl" --docTypes "product,blog-post"
```


## How to build

- install [Stack](http://haskellstack.org)
- run `stack build`


## Troubleshooting

### "commitBuffer: invalid argument"

On Windows, running `chcp 65001` before running prismic-backup solves the issue.

See https://jaspervdj.be/hakyll/tutorials/faq.html#hgetcontents-invalid-argument-or-commitbuffer-invalid-argument


## License

See [LICENSE](LICENSE)
