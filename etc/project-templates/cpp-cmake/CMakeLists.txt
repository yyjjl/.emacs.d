cmake_minimum_required(VERSION 2.8)
set(CMAKE_DISABLE_SOURCE_CHANGES ON)
set(CMAKE_DISABLE_IN_SOURCE_BUILD ON)
project(__PROJECT-NAME__)

set(CMAKE_CXX_FLAGS
  "${CMAKE_CXX_FLAGS} -std=c++14")

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin/)
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/lib/)
include_directories(${PROJECT_SOURCE_DIR}/include)

set(SOURCE_FILES
  src/main.cpp)

add_executable(main ${SOURCE_FILES})
