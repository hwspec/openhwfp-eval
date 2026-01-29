=======================

This repository is under construction.

It will include testbenches and use-case examples for open-source hardware floating-point libraries, along with scripts to perform area estimation using OpenROAD and to generate a web-based report.

Contents will be migrated from our private repository shortly.


### Dependencies

#### JDK 8 or newer

We recommend LTS releases Java 8 and Java 11. You can install the JDK as recommended by your operating system, or use the prebuilt binaries from [AdoptOpenJDK](https://adoptopenjdk.net/).

#### SBT

SBT is the most common built tool in the Scala community. You can download it [here](https://www.scala-sbt.org/index.html).  


Official sbt Debian Package Setup
```
sudo apt update
sudo apt install openjdk-17-jdk
```
``` 
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
________________________________________________________________________________________________________
If using Verilator 5.020 2024-01-01 rev (Debian 5.020-1) you may run into a a thread pool cleanup bug
where ChiselSim calls Verilator with -j 0 (use all threads) if this is the case, v5.044 will fix this!!! 
(*when using v5.044 the test FP_COS from openFloat will hang due to a width error in the source code ***Implementing fix***)
```
#Dependencies needed:
sudo apt install -y \
  git autoconf automake libtool \
  make g++ flex bison \
  libfl-dev zlib1g-dev \
  help2man

# Clone and Checkout
git clone https://github.com/verilator/verilator.git
cd verilator

git checkout tags/v5.044 -b v5.044build

# Build and Install 
autoconf
./configure --prefix=$HOME/verilator-5.044
make -j$(nproc)
make install

# Add to PATH 
echo 'export PATH=$HOME/verilator-5.044/bin:$PATH' >> ~/.bashrc
source ~/.bashrc

#Verify 
which verilator
verilator --version
```
________________________________________________________________________________________________________

### To run tests

```bash
$ sbt test
```




If you have any question please contact:
Kazutomo Yoshii <kazutomo@anl.gov> 
Connor Bohannon <cbohannon@anl.gov>

