#include <fstream>
#include <iostream>
#include <string>
#include <array>
#include <vector>
#include <iterator>

using namespace std;

vector<char> readBMP(const string &file){
    static constexpr size_t HEADER_SIZE = 54;
    
    ifstream bmp(file, ios::binary);

    array<char, HEADER_SIZE> header;
    bmp.read(header.data(), header.size());

    auto fileSize = *reinterpret_cast<u_int32_t *>(&header[2]);
    auto dataOffset = *reinterpret_cast<u_int32_t *>(&header[10]);
    auto width = *reinterpret_cast<u_int32_t *>(&header[18]);
    auto height = *reinterpret_cast<u_int32_t *>(&header[22]);
    auto depth = *reinterpret_cast<u_int32_t *>(&header[28]);

    cout << "fileSize: " << fileSize << endl;
    cout << "dataOffset: " << dataOffset << endl;
    cout << "width: " << width << endl;
    cout << "height: " << height << endl;
    cout << "depth: " << depth << endl;

    vector<char> img(dataOffset - HEADER_SIZE);
    bmp.read(img.data(), img.size());

    auto dataSize = ((width * 3 + 3) & (~3)) * height;
    img.resize(dataSize);
    bmp.read(img.data(), img.size());
    
    char temp = 0;

    for (auto i = dataSize - 4; i >= 0; i -= 3)
    {
	temp = img[i];
	img[i] = img[i+2];
	img[i+2] = temp;
	cout << "R: " << int(img[i] & 0xff) << " G: " << int(img[i+1] & 0xff) << " B: " << int(img[i+2] & 0xff) << endl;
    }

    return img;
}

int main(){ return 0; }
