sudo apt update
sudo apt install -y lsb-release wget software-properties-common gnupg

mkdir tmp
cd tmp

wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 16

sudo apt autoremove -y