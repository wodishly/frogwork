#version 410

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 uv;
out vec2 v_uv;

uniform mat4 u_projection_matrix;

void main() {
  v_uv = uv;
  gl_Position = u_projection_matrix * position;
}
