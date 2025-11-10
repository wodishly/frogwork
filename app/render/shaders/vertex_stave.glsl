#version 410

in vec3 position;
in vec2 uv;
out vec2 v_uv;

uniform mat4 u_projection_matrix;

void main() {
  v_uv = uv;
  gl_Position = vec4(position, 1.0);
}
