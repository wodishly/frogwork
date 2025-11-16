#version 410

layout(location = 0) in vec4 position;

out vec4 v_position;

void main() {
  v_position = position;
  gl_Position = position;
}
