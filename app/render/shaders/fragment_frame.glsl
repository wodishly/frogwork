#version 410

precision highp sampler2D;

in vec4 v_position;
out vec4 f_color;


void main() {
  f_color = vec4(vec3(v_position), 
    int(abs(v_position.x - round(v_position.x)) <= 0.01)
    + int(abs(v_position.y - round(v_position.y)) <= 0.01)
    + int(abs(v_position.z - round(v_position.z)) <= 0.01)
    >= 2 ? 1 : 0);
//  );
}
