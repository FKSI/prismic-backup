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
- find URLs in documents, fetch them and store data in a `_rc` directory


## How to use

Run `stack exec prismic-backup -- [PARAMS]` (or `./prismic-backup [PARAMS]` if you only have a binary).

### Parameters description

- `endpoint`: *(string)* API endpoint of the repository. For example: `https://lesbonneschoses.prismic.io/api/documents/search` (the `/documents/search` part is necessary)
- `ref`: *(string)* reference to use
- `docTypes`: *(string)* a comma-separated list of document types
- `output`: *(optional path, default = "./output")* path to the backup directory (will be created if it does not exist)

### Example

```
stack exec prismic-backup -- --endpoint "https://lesbonneschoses.prismic.io/api/documents/search" --ref "UlfoxUnM08QWYXdl" --docTypes "product,blog-post"
```
```
├───blog-post
│       UlfoxUnM0wkXYXbi.json
│       UlfoxUnM0wkXYXbl.json
│       UlfoxUnM0wkXYXbm.json
│       UlfoxUnM0wkXYXbt.json
│       UlfoxUnM0wkXYXbu.json
│       UlfoxUnM0wkXYXbX.json
│
├───product
│       UlfoxUnM0wkXYXbD.json
│       UlfoxUnM0wkXYXbe.json
│       UlfoxUnM0wkXYXbF.json
│       UlfoxUnM0wkXYXbg.json
│       UlfoxUnM0wkXYXbH.json
│       UlfoxUnM0wkXYXbI.json
│       UlfoxUnM0wkXYXbj.json
│       UlfoxUnM0wkXYXbk.json
│       UlfoxUnM0wkXYXbM.json
│       UlfoxUnM0wkXYXbO.json
│       UlfoxUnM0wkXYXbS.json
│       UlfoxUnM0wkXYXbT.json
│
└───_rc
    ├───i1.ytimg.com
    │   └───vi
    │       └───Ye78F3-CuXY
    │               hqdefault.jpg
    │
    └───prismic-io.s3.amazonaws.com
        └───lesbonneschoses
                0207c31d34736b9ab91a4d9982b3c131bccd738c.png
                0320bdf82034dfdf3d42964fef1703e1ad4f8fdb.png
                046a78c6d2f6da6fab9fe6fd8c9979a161bf6790.png
                06f093829de711ab835fd5a32d8a927717291102.jpg
                076279117b990effe56c1c487181ee7feabbdebc.png
                163483658e10f5bad8ee69c29ed313699ea792b8.jpg
                16f035668d3613d5330ce28880c5f337b5d71c5f.jpg
                206f3e7f2a865fa7b1c3826fbbba923361cdb038.png
                27108a480a1dca3012af843095443aaba75dfb48.jpg
                2d21ec2fc5bcde736867360885c291770d9ec0c4.jpg
                31afac1e1c90635db342fb1a38105829409f3111.jpg
                3b1b05c376cc2a125c7f9e7f4f2d1ab8659c950a.jpg
                3dd3cb44fe52c9f565d9d2d7b98cba87bac0d652.jpg
                4650457e3780a92119043c0629d9535708e7819a.png
                46608a915642fc1b71d75a5d36e2069e694e4021.png
                4f9baff1349940154a72f06f88332b5800084e5d.png
                51e5989df54c2141a379725060148a057e8f0541.png
                52fdf7c321edf0c6da21b462c872d4c84b6e9fc2.png
                57739050aced1fff635b72eaa82eb593d7030a8a.png
                59e9f323d8356159f585086e18c71bc8f79eff11.png
                604400b41b2e275ee766bd69b69b33734043aa38.png
                66387515268f9da1e447d009b46d8134a8e679cf.jpg
                71925289ed70e89812a4040de310f0a8b97a2375.jpg
                76cee6100507c4d01032221d343f5209c3e57f74.png
                78ef921308fe727a88530a76f6ab6e1123cdd147.png
                79a7a068ceb47892ac8de2480e878962d8b93cc6.png
                7c337a45be34e130923ebaa77297dd8e9bd7c797.jpg
                7c80c61c0904e805df675c01c8496edab270de4a.png
                8181933ff2f5032daff7d732e33a3beb6f57e09f.jpg
                890dfdf39b3f54ab84d3293010c81d53d9504deb.jpg
                8baee1ea007e292aa15a49f3aec5c040839eeb70.jpg
                8e03ece7a49a73d18af752f6f3f958f48423118f.jpg
                90954eac02861c4134431e3acb4f27938c5c5ce5.png
                a1d6d4ccdfd3dc4b20bdc5e2b27ccde05c406b6a.png
                a3ee80eab6dcf7e530a9b2c58779ab6b7164732f.jpg
                a5e3e7b0658ed2b15e4fe72825f6039c37df00b1.png
                a6a5bd25b4afd8fadd37e9f58de3e9bb9161ca77.png
                acc69618fab22be90aed95b39b91b62a80595a6b.png
                ad08d7e3215ed0e8ba495296b004f5883cb5cf50.jpg
                b12819d149a0e6c6cd6717dba4b2c6a1bdafa9d4.png
                b4ff61d17daa4a89f127d5db1e103d912e74a2c6.png
                b6d184431d8f2902b97983e6469ffc577da751b3.png
                bf6819a34ff187c5a59d781351a91834c92b7ebf.png
                c38f9e5a1a6c43aa7aae516c154013a2cee2bc75.jpg
                d83e12ae56083432dfea2a479a1c961412a25fc1.jpg
                e3256d8d19202eba7c4bb1292aaa01de5459f698.png
                e357ac7ace1734fd4af978f4c948286f6fba0f81.png
                e61ddb222a5b814006c4111c07ffd35d765672c1.jpg
                ee7b984b98db4516aba2eabd54ab498293913c6c.jpg
                ef01b2f46bac111bd7f53549bf9287a71b7514bf.jpg
                f606ad513fcc2a73b909817119b84d6fd0d61a6d.png
                f972f42a152ac9d9aff8627dbc0b95f743038eba.png
                fe4f9379ee325456992d48204b8d94aeb60cc976.png
                fe939eeb67713903f7f68c31efc98e52e1c7655d.png
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
