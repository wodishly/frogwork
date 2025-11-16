#version 410

precision highp sampler2D;

in vec4 v_position;
out vec4 f_color;

void main() {
  float edge = 1.0/80;
  vec3 whelk = (v_position.x < -7.0/8+edge
             || v_position.x >  7.0/8-edge
             || v_position.y > -2.0/8-edge
             || v_position.y < -7.0/8+edge)
    ? vec3(203.0, 203.0, 255.0)
    : vec3( 53.0,  59.0,  79.0);
  f_color = vec4(whelk/255, 7.0/8);
}
