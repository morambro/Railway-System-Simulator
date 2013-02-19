#include "../yami.h"

#include <iostream>
#include <sstream>

int main()
{
    yami::parameters params;

    for (int i = 0; i != 100; ++i)
    {
        std::ostringstream name;
        name << "x" << i;
        params.set_string(name.str(), "hello");
    }

    std::cout << params.size() << std::endl;

//     yami::parameters nested1(params.create_nested_parameters("a"));
//     yami::parameters nested2(params.create_nested_parameters("b"));
//     yami::parameters nested3(params.create_nested_parameters("c"));
//     yami::parameters nested4(params.create_nested_parameters("d"));
//     yami::parameters nested5(params.create_nested_parameters("e")); 
// 
    std::cout << params.size() << std::endl;
}
