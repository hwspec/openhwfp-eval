=======================

This repository is under construction.

It will include testbenches and use-case examples for open-source hardware floating-point libraries, along with scripts to perform area estimation using OpenROAD and to generate a web-based report.

Contents will be migrated from our private repository shortly.


### Dependencies

#### JDK 8 or newer

We recommend LTS releases Java 8 and Java 11. You can install the JDK as recommended by your operating system, or use the prebuilt binaries from [AdoptOpenJDK](https://adoptopenjdk.net/).

#### SBT

SBT is the most common built tool in the Scala community. You can download it [here](https://www.scala-sbt.org/index.html).  

```Debian package 
   echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
   echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
   curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
   sudo apt-get update
   
   sudo apt-get install sbt

   sbt --version
```
   
#### Verilator

Tested with v5.010. Below is a local build instruction:

```bash
git clone https://github.com/verilator/verilator.git && cd verilator
git checkout tags/v5.010  -b v5.010build
autoconf
./configure --prefix=__INSTALLDIR__  # replace __INSTALLDIR__
make
make install
```

### To run tests

```bash
$ sbt test
```


Please contact Kazutomo Yoshii <kazutomo@anl.gov> if you have any question.


