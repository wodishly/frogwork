#version 410

precision highp sampler2D;

in vec4 v_position;
out vec4 f_color;


void main() {
    f_color = vec4(vec3(v_position), 1);
//  f_color = vec4(0, 1, 0,//vec3(v_position),
//    int(abs(v_position.x) == 0.975)
//    + int(abs(v_position.y) > 0.975)
//    + int(abs(v_position.z) > 0.975)
//    >= 2 ? 1 : 0);
}
