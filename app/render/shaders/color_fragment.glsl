#version 410

layout(location = 0) out vec4 diffuseColor;

in vec4 v_position;
out vec4 f_color;

uniform float u_time;

void main() {
  return;
  vec2 xz = floor(mod(vec2(
    v_position.x+(mod(u_time, 2.0) < 1 ? u_time : 0),
    v_position.z+(mod(u_time, 2.0) >= 1 ? u_time : 0) + 1.0
  ), 2.0));
  float value = xz.x + xz.y == 1.0 ? 1.0 : 0.0;
  f_color = vec4(length(v_position)*vec3(value * (0.5 + 0.5*sin(u_time)), value * (0.5 + 0.5*cos(u_time)), 0.5), 1.0);
}
