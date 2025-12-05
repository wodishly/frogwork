#version 410

precision highp sampler2D;

in vec4 v_position;
out vec4 f_color;


void main() {
    f_color = vec4(v_position.y, v_position.y, v_position.y*2, 1);
}
